module AugsLink.Service.Handlers.PutScrapeSong 
  (
    scrape
  ) where 

import Data.Text
import Servant

import AugsLink.Core.API
import AugsLink.Service.API

scrape :: RoomId -> SongId -> ScrapeSongRequest -> Handler Text
scrape rId sId req = do undefined
