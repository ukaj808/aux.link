module AugsLink.Service.Handlers.PostUploadSong where 

import Servant
import qualified Data.Text as T
import Servant.Multipart

upload :: MultipartData Mem -> Handler T.Text
upload dt = do undefined
