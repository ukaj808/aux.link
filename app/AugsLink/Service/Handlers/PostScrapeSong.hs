module AugsLink.Service.Handlers.PostScrapeSong where 

import Servant
import qualified Data.Text as T
import AugsLink.Service.API (ScrapeSongRequest)

scrape :: T.Text -> ScrapeSongRequest -> Handler T.Text
scrape req = do undefined
