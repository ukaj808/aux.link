module AugsLink.Service.Handlers.PostHome 
  ( create
  ) where

import Control.Monad.IO.Class ( liftIO )
import Servant
import AugsLink.Service.Room (RoomServer (createRoom))

create :: IO (RoomServer IO) -> Handler (Headers '[Header "Location" [Char]] [Char])
create rServer = do
  roomId <- createRoom rServer
  return $ addHeader (genLocation roomId) roomId

genLocation :: String -> String
genLocation roomId = "http://localhost:8080/" ++ roomId
