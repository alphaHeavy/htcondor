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
  , Network.HTCondor.Submit.Language.error
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

import Network.HTCondor.Submit.Language
import Network.HTCondor.Submit.Types
