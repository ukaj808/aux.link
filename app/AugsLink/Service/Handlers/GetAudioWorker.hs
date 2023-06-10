{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Handlers.GetAudioWorker 
  ( 
    audioWorkerHandler
  ) where

import CommandLine
import Control.Monad.IO.Class
import Data.Text
import Servant

import qualified Data.ByteString.Lazy as LBS

import AugsLink.Service.API

audioWorkerHandler :: CLArgs 
  -> Handler 
       (
         Headers 
           '[
              Header "Cross-Origin-Opener-Policy" Text
            , Header "Cross-Origin-Embedder-Policy" Text
            ] 
         StaticJs
       )

audioWorkerHandler opts = do
  audioWorkerFile <- liftIO $ LBS.readFile $ audioWorkerPath opts 
  return 
    (
      addHeader "same-origin"  $
      addHeader "require-corp" $
      StaticJs audioWorkerFile
    )
