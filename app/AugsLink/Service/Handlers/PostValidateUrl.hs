module AugsLink.Service.Handlers.PostValidateUrl
(
  validateHandler
) where

import Control.Monad.Cont
import Data.Text
import Servant

import AugsLink.Service.API
import Commons.YtDlp

validateHandler :: ValidateUrlRequest -> Handler Text
validateHandler req = liftIO $ ytdlpValid "yt-dlp" (url req)
  