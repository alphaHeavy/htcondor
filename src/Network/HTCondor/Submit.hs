{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.HTCondor.Submit
  ( LogEvent(..)
  , EventType(..)
  , submit
  , submitAndWait
  , wait

    -- * Basic Commands
  , executable
  , executable_
  , arguments
  , environment
  , Network.HTCondor.Submit.error
  , getenv
  , input
  -- , Submit.log
  -- , logXml
  , notification
  , notifyUser
  , output
  , priority
  , queue
  , queue_
  , universe

    -- * Commands for Matchmaking
  , rank
  , requestCPUs
  , requestDisk
  , requestMemory
  , requirements

    -- * File Transfer Commands
  , dontEncryptInputFiles
  , dontEncryptOutputFiles
  , encryptInputFiles
  , encryptOutputFiles
  , outputDestination
  , shouldTransferFiles
  , skipFilechecks
  , streamError
  , streamInput
  , streamOutput
  , transferExecutable
  , transferInputFiles
  , transferOutputFiles
  , transferOutputRemaps
  , whenToTransferOutput
  ) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Lazy
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import Data.Conduit
import qualified Data.Conduit.Binary as Cb
import qualified Data.Conduit.List as Cl
import qualified Data.Conduit.Text as Ct
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (Handle, hClose, openBinaryTempFile)
import System.Process (readProcess)
import Text.Parsec as Parsec hiding ((<|>))
import Text.Parsec.Pos (initialPos)
import Text.Parsec.Text ()

newtype CondorT m a = CondorT {runCondor :: StateT [Map Text [Text]] m a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadState [Map Text [Text]])

type Condor a = CondorT Identity a

data CondorNotification
  = NotifyAlways
  | NotifyComplete
  | NotifyError
  | NotifyNever

data CondorUniverse
  = CondorVanilla
  | CondorStandard
  | CondorScheduler
  | CondorLocal
  | CondorGrid
  | CondorJava
  | CondorVM

modifyHead :: MonadState [t] m => (t -> t) -> m ()
modifyHead f = modify (\ (x:xs) -> f x:xs)

insertBool :: Monad m => Text -> Bool -> CondorT m ()
insertBool key f = modifyHead (Map.insert key [if f then "True" else "False"])

type ClusterId = Int
type ProcId = Int
type NodeNumber = Int

data LogEvent = LogEvent
  { logEventType       :: EventType
  , logEventClusterId  :: ClusterId
  , logEventProcId     :: ProcId
  , logEventNodeNumber :: NodeNumber
  , logEventText       :: Text
  } deriving Show

data EventType
  = JobSubmitted
  | JobExecuting
  | ErrorInExecutable
  | JobCheckpointed
  | JobEvicted
  | JobTerminated
  | ImageSizeUpdated
  | ShadowException
  | JobAborted
  | JobSuspended
  | JobHeld
  | JobReleased
    deriving Show

eventType :: Monad m => ParsecT Text u m EventType
eventType =
      try (string "000" *> pure JobSubmitted)
  <|> try (string "001" *> pure JobExecuting)
  <|> try (string "002" *> pure ErrorInExecutable)
  <|> try (string "003" *> pure JobCheckpointed)
  <|> try (string "004" *> pure JobEvicted)
  <|> try (string "005" *> pure JobTerminated)
  <|> try (string "006" *> pure ImageSizeUpdated)
  <|> try (string "007" *> pure ShadowException)
  <|> try (string "009" *> pure JobAborted)
  <|> try (string "010" *> pure JobSuspended)
  <|> try (string "012" *> pure JobHeld)
  <|> try (string "013" *> pure JobReleased)

parseReads :: (Read a, Stream s m t) => String -> ParsecT s u m a
parseReads s
  | [(val, "")] <- reads s = pure val
  | otherwise = unexpected "no parse"

threeDigits :: Monad m => ParsecT Text () m Int
threeDigits = Parsec.count 3 digit >>= parseReads

logParserLine :: Monad m => ParsecT Text () m LogEvent
logParserLine = spaces *> logParser <* spaces

logParser :: Monad m => ParsecT Text () m LogEvent
logParser = LogEvent
  <$> eventType
  <*> (space *> char '(' *> threeDigits)
  <*> (char '.' *> threeDigits)
  <*> (char '.' *> threeDigits <* char ')' <* space)
  <*> (Text.pack <$> Parsec.many anyChar)

-- |
-- Break up the condor_submit logfile by splitting on "..." lines
logChunkSplitter :: MonadThrow m => GInfConduit ByteString m [Text]
logChunkSplitter = Cb.lines >+> Ct.decode Ct.utf8 >+> step [] where
  revyield xs = unless (null xs) $ yield (reverse xs)
  step xs = do
    mval <- awaitE
    case mval of
      Right val
        | Text.strip val == "..." -> do
            revyield xs
            step []

        | otherwise -> step (val:xs)

      Left val -> do
        revyield xs
        return val

logPipe :: Monad m => SourcePos -> GInfConduit [Text] m LogEvent
logPipe !pos = do
  mval <- awaitE
  case mval of
    Right val ->
      case runParser (setPosition pos >> logParserLine) () "" (Text.unlines val) of
        Right val' -> do
          yield val'
          -- 1 extra for the "..."
          let pos' = incSourceLine pos (1+length val)
          logPipe pos'
        Left  err  -> fail $ show err
    Left val -> return val

binaryTempFile :: MonadResource m => String -> m (ReleaseKey, FilePath, Handle)
binaryTempFile template = do
  let cleanupTempFile (f, h) = do
        hClose h
        removeFile f
  tmpdir <- liftIO getTemporaryDirectory

  (releaseKey, (logFile, logHandle)) <- allocate
    (openBinaryTempFile tmpdir template)
    cleanupTempFile

  return (releaseKey, logFile, logHandle)

submit :: MonadResource m => Condor () -> GSource m LogEvent
submit c = do
  (logKey, logFile, logHandle) <- binaryTempFile "hs-htcondor.log"
  let script = pretty (Network.HTCondor.Submit.log logFile >> c)
  -- liftIO $ putStrLn . Text.unpack $ script
  -- res1 <- liftIO $ readProcess "condor_submit" ["-verbose"] (Text.unpack script)
  -- liftIO $ print res1
  _ <- liftIO . readProcess "condor_submit" [] $ Text.unpack script

  -- rely on the sink to exit
  _ <- forever (Cb.sourceHandle logHandle >> liftIO (threadDelay 1000000))
    >+> logChunkSplitter
    >+> logPipe (initialPos logFile)

  release logKey

submitAndWait :: Condor () -> IO ()
submitAndWait c =
  runResourceT $ submit c $$ wait >+> Cl.sinkNull

-- |
-- Wait for all submitted jobs to complete
wait :: forall m . Monad m => GConduit LogEvent m LogEvent
wait = step False 0 where
  step :: Bool -> Int -> GConduit LogEvent m LogEvent
  step True 0 = return ()
  step seen i = do
    mval <- await
    case mval of
      Nothing -> return ()
      Just val -> do
        yield val
        case logEventType val of
          JobSubmitted      -> step True (i+1)
          JobTerminated     -> step seen (i-1)
          JobAborted        -> step seen (i-1)
          ErrorInExecutable -> step seen (i-1)
          _                 -> step seen i

arguments :: Monad m => [Text] -> CondorT m ()
arguments = modifyHead . Map.insert "arguments"

environment :: Monad m => Map Text Text -> CondorT m ()
environment = modifyHead . Map.insert "environment" . fmap x . Map.toList where
  x (k, v) = k <> "=" <> v

error :: Monad m => FilePath -> CondorT m ()
error errorPath = modifyHead (Map.insert "error" [Text.pack errorPath])

executable :: Monad m => FilePath -> [Text] -> CondorT m ()
executable executablePath args = do
  executable_ executablePath
  arguments args

executable_ :: Monad m => FilePath -> CondorT m ()
executable_ executablePath = modifyHead (Map.insert "executable" [Text.pack executablePath])

getenv :: Monad m => Bool -> CondorT m ()
getenv = insertBool "getenv"

input :: Monad m => FilePath -> CondorT m ()
input inputPath = modifyHead (Map.insert "input" [Text.pack inputPath])

log :: Monad m => FilePath -> CondorT m ()
log logPath = modifyHead (Map.insert "log" [Text.pack logPath])

-- logXml :: Monad m => Bool -> CondorT m ()
-- logXml = insertBool "log_xml"

notification :: Monad m => CondorNotification -> CondorT m ()
notification n = modifyHead (Map.insert "notification" [format n]) where
  format NotifyAlways   = "Always"
  format NotifyComplete = "Complete"
  format NotifyError    = "Error"
  format NotifyNever    = "Never"

notifyUser :: Monad m => Text -> CondorT m ()
notifyUser emailAddress = modifyHead (Map.insert "notifyUser" [emailAddress])

output :: Monad m => FilePath -> CondorT m ()
output outputPath = modifyHead (Map.insert "output" [Text.pack outputPath])

priority :: Monad m => Int -> CondorT m ()
priority priorityLevel = modifyHead (Map.insert "priority" [Text.pack $ show priorityLevel])

queue_ :: Monad m => CondorT m ()
queue_ = queue 1

queue :: Monad m => Int -> CondorT m ()
queue x = modify (replicate x Map.empty ++)

universe :: Monad m => CondorUniverse -> CondorT m ()
universe n = modifyHead (Map.insert "universe" [format n]) where
  format CondorVanilla   = "vanilla"
  format CondorStandard  = "standard"
  format CondorScheduler = "scheduler"
  format CondorLocal     = "local"
  format CondorGrid      = "grid"
  format CondorJava      = "java"
  format CondorVM        = "vm"


rank :: Monad m => Text -> CondorT m ()
rank str = modifyHead (Map.insert "rank" [str])

requestCPUs :: Monad m => Int -> CondorT m ()
requestCPUs num = modifyHead (Map.insert "requestCPUs" [Text.pack $ show num])

requestDisk :: Monad m => Int -> CondorT m ()
requestDisk num = modifyHead (Map.insert "requestDisk" [Text.pack $ show num])

requestMemory :: Monad m => Int -> CondorT m ()
requestMemory num = modifyHead (Map.insert "requestMemory" [Text.pack $ show num])

requirements  :: Monad m => Text -> CondorT m ()
requirements str = modifyHead (Map.insert "requirements" [str])


dontEncryptInputFiles :: Monad m => [FilePath] -> CondorT m ()
dontEncryptInputFiles = modifyHead . Map.insert "dont_encrypt_input_files" . fmap Text.pack

dontEncryptOutputFiles :: Monad m => [FilePath] -> CondorT m ()
dontEncryptOutputFiles = modifyHead . Map.insert "dont_encrypt_output_files" . fmap Text.pack

encryptInputFiles :: Monad m => [FilePath] -> CondorT m ()
encryptInputFiles = modifyHead . Map.insert "encrypt_input_files" . fmap Text.pack

encryptOutputFiles :: Monad m => [FilePath] -> CondorT m ()
encryptOutputFiles = modifyHead . Map.insert "encrypt_output_files" . fmap Text.pack

outputDestination :: Monad m => Text -> CondorT m ()
outputDestination str = modifyHead (Map.insert "output_destination" [str])

shouldTransferFiles :: Monad m => Maybe Bool -> CondorT m ()
shouldTransferFiles n = modifyHead (Map.insert "should_transfer_files" [format n]) where
  format Nothing      = "IF_NEEDED"
  format (Just True)  = "YES"
  format (Just False) = "NO"

skipFilechecks :: Monad m => Bool -> CondorT m ()
skipFilechecks = insertBool "skip_filechecks"

streamError :: Monad m => Bool -> CondorT m ()
streamError = insertBool "stream_error"

streamInput :: Monad m => Bool -> CondorT m ()
streamInput = insertBool "stream_input"

streamOutput :: Monad m => Bool -> CondorT m ()
streamOutput = insertBool "stream_output"

transferExecutable :: Monad m => Bool -> CondorT m ()
transferExecutable = insertBool "transfer_executable"

transferInputFiles :: Monad m => [FilePath] -> CondorT m ()
transferInputFiles = modifyHead . Map.insert "transfer_input_files" . fmap Text.pack

transferOutputFiles :: Monad m => [FilePath] -> CondorT m ()
transferOutputFiles = modifyHead . Map.insert "transfer_output_files" . fmap Text.pack

transferOutputRemaps :: Monad m => Map FilePath FilePath -> CondorT m ()
transferOutputRemaps m = modifyHead (Map.insert "transfer_output_remaps" [format m]) where
  format = Text.intercalate ";" . fmap (\ (k, v) -> Text.pack k <> " = " <> Text.pack v) . Map.toList

whenToTransferOutput :: Monad m => [FilePath] -> CondorT m ()
whenToTransferOutput = modifyHead . Map.insert "when_to_transfer_output" . fmap Text.pack


pretty :: Condor () -> Text
pretty (CondorT st) =
  let s = reverse . drop 1 $ execState st [Map.empty]
      m = Text.unlines . fmap (\ (k, v) -> k <> " = " <> Text.intercalate ", " v) . Map.toList
  in foldMap (\ x -> m x <> "queue\n") s
