module AugsLink.Service.Handlers.PostHome 
  ( create
  ) where

import Servant
import Control.Monad.Cont (MonadIO(liftIO))
import AugsLink.Service.Room

create :: RoomControl IO -> Handler (Headers '[Header "Location" [Char]] [Char])
create rc = do
  rId <- liftIO $ createRoom rc
  return $ addHeader (genLocation rId) rId

genLocation :: RoomId -> String
genLocation roomId = "http://localhost:8080/" ++ roomId
