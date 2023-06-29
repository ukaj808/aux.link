module AugsLink.Service.Handlers.PostValidateScrapeUrl
(
  validateHandler
) where

import Data.Text
import Servant

import AugsLink.Core.API
import AugsLink.Service.API
import Control.Monad.Cont

validateHandler :: ScrapeUrlValidateRequest -> Handler Text
validateHandler = undefined