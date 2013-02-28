{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Network.HTCondor.Submit.LogParser
  ( submitLogPipe
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import Data.Conduit
import qualified Data.Conduit.Binary as Cb
import qualified Data.Conduit.Text as Ct
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parsec as Parsec hiding ((<|>))
import Text.Parsec.Pos (initialPos)
import Text.Parsec.Text ()

import Network.HTCondor.Submit.Types

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

logParserPipe :: Monad m => SourcePos -> GInfConduit [Text] m LogEvent
logParserPipe !pos = do
  mval <- awaitE
  case mval of
    Right val ->
      case runParser (setPosition pos >> logParserLine) () "" (Text.unlines val) of
        Right val' -> do
          yield val'
          -- 1 extra for the "..."
          let pos' = incSourceLine pos (1+length val)
          logParserPipe pos'
        Left  err  -> fail $ show err
    Left val -> return val

submitLogPipe :: MonadThrow m => FilePath -> GInfConduit ByteString m LogEvent
submitLogPipe logFile =
      logChunkSplitter
  >+> logParserPipe (initialPos logFile)
