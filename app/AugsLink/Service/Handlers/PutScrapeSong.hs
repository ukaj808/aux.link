module AugsLink.Service.Handlers.PutScrapeSong 
  (
    scrape
  ) where 

import Data.Text
import Servant

import AugsLink.Service.API (ScrapeSongRequest)

scrape :: Text -> ScrapeSongRequest -> Handler Text
scrape req = do undefined
