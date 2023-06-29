module AugsLink.Service.Handlers.PutScrapeSong 
(
  scrapeHandler
) where

import Data.Text
import Servant

import AugsLink.Core.API
import AugsLink.Service.API ( ScrapeUploadRequest )
import Control.Monad.Cont

scrapeHandler :: Registry IO -> RoomId -> UserId -> ScrapeUploadRequest -> Handler Text
scrapeHandler rr rId uId req = do
  maybeRoom <- liftIO $ getRoom rr rId
  case maybeRoom of
    Nothing -> throwError err404
    Just room -> do
      return $ pack ""