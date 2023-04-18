module AugsLink.Service.Handlers.PutScrapeSong 
  (
    scrapeHandler
  ) where 

import Data.Text
import Servant

import AugsLink.Core.API
import AugsLink.Service.API

scrapeHandler :: RoomId -> SongId -> ScrapeSongRequest -> Handler Text
scrapeHandler rId sId req = do undefined
