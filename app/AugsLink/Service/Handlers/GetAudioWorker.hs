module AugsLink.Service.Handlers.GetAudioWorker 
  ( 
    audioWorkerHandler
  ) where

import CommandLine
import Control.Monad.IO.Class
import Servant

import qualified Data.ByteString.Lazy as LBS

import AugsLink.Service.API

audioWorkerHandler :: CLArgs -> Handler StaticJs

audioWorkerHandler opts = do
  audioWorkerFile <- liftIO $ LBS.readFile $ audioWorkerPath opts 
  return $ StaticJs audioWorkerFile
    
