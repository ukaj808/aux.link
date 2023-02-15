module AugsLink.Service.Handlers.PostHome 
  ( create
  ) where

import Servant
import Control.Monad.Cont (MonadIO(liftIO))
import AugsLink.Service.Room
import Data.UUID (toString)

create :: Registry IO -> Handler (Headers '[Header "Location" [Char]] [Char])
create rr = do
  rId <- liftIO $ createRoom rr
  return $ addHeader (genLocation rId) (toString rId)

genLocation :: RoomId -> String
genLocation roomId = "http://localhost:8080/" ++ toString roomId
