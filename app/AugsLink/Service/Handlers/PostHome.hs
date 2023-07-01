module AugsLink.Service.Handlers.PostHome 
  ( 
    createHandler
  ) where

import Data.Text
import Control.Monad.IO.Class
import Servant

import AugsLink.Core.API

createHandler :: Registry IO -> Handler (Headers '[Header "Location" Text] NoContent)
createHandler rr = do
  rId <- liftIO $ createRoom rr
  return $ addHeader (genLocation rId) NoContent

genLocation :: RoomId -> Text
genLocation = append (pack "http://localhost:8080/")
