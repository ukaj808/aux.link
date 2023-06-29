module AugsLink.Service.Handlers.PostValidateScrapeUrl
(
  validateHandler
) where

import Control.Monad.Cont
import Data.Text
import Servant

import AugsLink.Core.API
import AugsLink.Service.API
import Commons.YoutubeDl

validateHandler :: ScrapeUrlValidateRequest -> Handler Text
validateHandler req = do
  valid <- liftIO $ isValidUrl (u req)
  if valid
    then return $ pack ""
    else throwError err400