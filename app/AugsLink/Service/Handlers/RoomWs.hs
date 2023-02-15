module AugsLink.Service.Handlers.RoomWs 
  ( enter
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Network.WebSockets as WS
import Servant

import AugsLink.Service.Room (enterRoom, Registry (getRoom), Room (..) )
import Data.UUID (fromString)

-- Should not terminate until the room is no longer required; because if it
-- does then the ws connection will close on the browser
enter :: Registry IO -> String -> WS.PendingConnection -> Handler ()
enter rr eId pc = liftIO $ do

  let rId = case fromString eId of
              Just roomId -> roomId
              Nothing -> error "Invalid unique id"

  r <- getRoom rr rId

  let room = case r of
               Just rm -> rm
               Nothing -> error "Room does not exist"
  enterRoom room pc

