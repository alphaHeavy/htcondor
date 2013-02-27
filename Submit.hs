{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Submit
  ( submit
    -- * Basic Commands
  , arguments
  , environment
  , Submit.error
  , executable
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

    -- * Other crap
  , logParser
  , testString
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Lazy
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import System.Process (readProcess)
import Text.Parsec hiding ((<|>))
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

-- Submitting job(s).
-- 1 job(s) submitted to cluster 480.
-- blah :: Parsec 

type ClusterId = Int
type ProcId = Int
type NodeNumber = Int

data LogEvent
  = LogEvent EventType ClusterId ProcId NodeNumber Text
    deriving Show

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

testString :: Text
testString = Text.unlines
  [ "000 (018.000.000) 07/17 09:15:52 Job submitted from host: <10.0.1.2:49039>"
  , "..."
  , "001 (018.000.000) 07/17 09:16:02 Job executing on host: <10.0.2.4:37941>"
  , "..."
  , "006 (018.000.000) 07/17 09:16:03 Image size of job updated: 75000"
  , "\t\t0  -  MemoryUsage of job (MB)"
  , "\t\t0  -  ResidentSetSize of job (KB)"
  , "..."
  ]

eventType :: Parsec Text u EventType
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

logParser' :: Parsec Text () LogEvent
logParser' = spaces *> logParser <* spaces

logParser :: Parsec Text () LogEvent
logParser = LogEvent
  <$> eventType
  <*> pure 0
  <*> pure 0
  <*> pure 0
  <*> (Text.pack <$> Text.Parsec.many anyChar)

submit :: Condor () -> IO ()
submit c = do
  let script = pretty (Submit.log "/tmp/foo.log" >> c)
  putStrLn . Text.unpack $ script
  res1 <- readProcess "condor_submit" ["-verbose"] (Text.unpack script)
  print res1
  res2 <- readProcess "condor_wait" ["/tmp/foo.log"] ""
  print res2
  return ()

arguments :: Monad m => [Text] -> CondorT m ()
arguments = modifyHead . Map.insert "arguments"

environment :: Monad m => Map Text Text -> CondorT m ()
environment = modifyHead . Map.insert "environment" . fmap x . Map.toList where
  x (k, v) = k <> "=" <> v

error :: Monad m => FilePath -> CondorT m ()
error errorPath = modifyHead (Map.insert "error" [Text.pack errorPath])

executable :: Monad m => FilePath -> CondorT m ()
executable executablePath = modifyHead (Map.insert "executable" [Text.pack executablePath])

getenv :: Monad m => Bool -> CondorT m ()
getenv f = modifyHead (Map.insert "getenv" [if f then "True" else "False"])

input :: Monad m => FilePath -> CondorT m ()
input inputPath = modifyHead (Map.insert "input" [Text.pack inputPath])

log :: Monad m => FilePath -> CondorT m ()
log logPath = modifyHead (Map.insert "log" [Text.pack logPath])

logXml :: Monad m => Bool -> CondorT m ()
logXml f = modifyHead (Map.insert "log_xml" [if f then "True" else "False"])

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
queue_ = modify (Map.empty:)

queue :: Monad m => Int -> CondorT m ()
queue x = modify ((replicate x Map.empty)++)

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


test :: Condor ()
test = do
  arguments ["test", "ok"]
  queue_
  executable "/tmp/foo"
  queue_

pretty :: Condor () -> Text
pretty (CondorT st) =
  let s = reverse . drop 1 $ execState st [Map.empty]
      m = Text.unlines . fmap (\ (k, v) -> k <> " = " <> Text.intercalate ", " v) . Map.toList
  in foldMap (\ x -> m x <> "queue\n") s
