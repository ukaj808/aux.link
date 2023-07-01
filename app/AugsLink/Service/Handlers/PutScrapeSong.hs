module AugsLink.Service.Handlers.PutScrapeSong 
(
  scrapeHandler
) where

import Data.Text
import Servant

import AugsLink.Core.API
import AugsLink.Service.API
import Control.Monad.Cont

scrapeHandler :: Registry IO -> RoomId -> UserId -> ScrapeUrlUploadRequest -> Handler NoContent
scrapeHandler rr rId uId req = do
  maybeRoom <- liftIO $ getRoom rr rId
  case maybeRoom of
    Nothing -> throwError err404
    Just room -> do
      uploadSong room uId (UrlScrapeUpload $ url req)
      return NoContent