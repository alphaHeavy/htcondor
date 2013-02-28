{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.HTCondor.Submit.Types
  ( CondorT(..)
  , Condor
  , LogEvent(..)
  , ClusterId
  , ProcId
  , NodeNumber
  , EventType(..)
  , CondorNotification(..)
  , CondorUniverse(..)
  ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State.Lazy
import Data.Map (Map)
import Data.Text (Text)

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
