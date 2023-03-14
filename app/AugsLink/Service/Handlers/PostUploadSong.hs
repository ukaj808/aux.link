module AugsLink.Service.Handlers.PostUploadSong where 

import Servant
import qualified Data.Text as T
import Servant.Multipart
import AugsLink.Core.API (Registry)

upload :: T.Text -> MultipartData Mem -> Handler T.Text
upload dt = do undefined
