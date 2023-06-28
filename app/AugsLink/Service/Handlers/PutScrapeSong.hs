module AugsLink.Service.Handlers.PutScrapeSong 
(
  scrapeHandler
) where

import Data.Text
import Servant

import AugsLink.Core.API
import AugsLink.Service.API

scrapeHandler :: Registry IO -> RoomId -> UserId -> Maybe Text -> ScrapeUploadRequest -> Handler Text
scrapeHandler = undefined