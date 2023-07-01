module AugsLink.Service.Handlers.PostValidateScrapeUrl
(
  validateHandler
) where

import Control.Monad.Cont
import Data.Text
import Servant

import AugsLink.Core.API
import AugsLink.Service.API
import Commons.YtDlp

validateHandler :: ScrapeUrlValidateRequest -> Handler Bool
validateHandler req = liftIO $ ytdlpValid "yt-dlp" (u req)
  