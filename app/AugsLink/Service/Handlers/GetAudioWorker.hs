{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Handlers.GetAudioWorker 
  ( 
    audioWorkerHandler
  ) where

import Control.Monad.IO.Class
import Servant

import CommandLine
import Data.Text
import AugsLink.Service.API
import qualified Data.ByteString.Lazy as Lazy

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
  liftIO $ print "auidohanldler"
  audioWorkerFile <- liftIO $ Lazy.readFile $ audioWorkerPath opts 
  return 
    (
      addHeader "same-origin"  $
      addHeader "require-corp" $
      StaticJs audioWorkerFile
    )
