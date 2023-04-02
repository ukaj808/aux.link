module AugsLink.Service.Handlers.RoomWs 
  ( 
    enter
  ) where

import Control.Monad.IO.Class
import Data.Text
import Servant

import qualified Network.WebSockets as WS

import AugsLink.Core.API

type instance Connection IO = WS.PendingConnection

-- Should not terminate until the room is no longer required; because if it
-- does then the ws connection will close on the browser
enter :: Registry IO -> Text -> WS.PendingConnection -> Handler ()
enter rr rId pc = liftIO $ do

  r <- getRoom rr rId

  let room = case r of
               Just rm -> rm
               Nothing -> error "Room does not exist"

  enterRoom room pc

