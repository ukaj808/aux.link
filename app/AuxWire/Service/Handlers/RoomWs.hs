module AuxWire.Service.Handlers.RoomWs 
  ( 
    enterHandler
  ) where

import Control.Monad.IO.Class
import Data.Text
import Servant

import qualified Network.WebSockets as WS

import AuxWire.Core.API

type instance Connection IO = WS.PendingConnection

-- Should not terminate until the room is no longer required; because if it
-- does then the ws connection will close on the browser
enterHandler :: Registry IO -> Text -> WS.PendingConnection -> Handler ()
enterHandler rr rId pc = liftIO $ do

  r <- getRoom rr rId

  let room = case r of
               Just rm -> rm
               Nothing -> error "Room does not exist"

  enterRoom room pc

