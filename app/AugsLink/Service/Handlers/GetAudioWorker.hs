module AugsLink.Service.Handlers.GetAudioWorker 
  ( 
    audioWorkerHandler
  ) where

import Control.Monad.IO.Class
import Servant

import qualified Data.Text.IO as T
import CommandLine
import Data.Text

audioWorkerHandler :: CLArgs -> Handler (
       Headers 
       '[
         Header "Cross-Origin-Opener-Policy" Text, 
         Header "Cross-Origin-Embedder-Policy" Text
        ] 
        Text
      )
audioWorkerHandler opts = do
  audioWorkerFile <- liftIO $ T.readFile $ audioWorkerPath opts 
  return 
    ( 
      addHeader "same-origin"       $
      addHeader "require-corp"      $
      audioWorkerFile
    )
