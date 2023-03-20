module AugsLink.Service.Handlers.PostHome 
  ( 
    create
  ) where

import Servant
import Control.Monad.IO.Class
import Data.Text
import Data.UUID

import AugsLink.Core.API

create :: Registry IO -> Handler (Headers '[Header "Location" Text] Text)
create rr = do
  rId <- liftIO $ createRoom rr
  return $ addHeader (genLocation rId) (toText rId)

genLocation :: RoomId -> Text
genLocation roomId = append (pack "http://localhost:8080/") $ toText roomId
