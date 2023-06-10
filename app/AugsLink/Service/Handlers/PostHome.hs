module AugsLink.Service.Handlers.PostHome 
  ( 
    createHandler
  ) where

import Data.Text
import Control.Monad.IO.Class
import Servant

import AugsLink.Core.API

createHandler :: Registry IO -> Handler (Headers '[Header "Location" Text] Text)
createHandler rr = do
  rId <- liftIO $ createRoom rr
  return $ addHeader (genLocation rId) rId

genLocation :: RoomId -> Text
genLocation = append (pack "http://localhost:8080/")
