module AugsLink.Service.Handlers.PostHome 
  ( create
  ) where

import Control.Monad.IO.Class ( liftIO )
import Servant

create :: Handler (Headers '[Header "Location" [Char]] [Char])
create = do
  roomId <- liftIO createRoom
  return $ addHeader (genLocation roomId) roomId

createRoom :: IO String
createRoom = return "123";

genLocation :: String -> String
genLocation roomId = "http://localhost:8080/" ++ roomId
