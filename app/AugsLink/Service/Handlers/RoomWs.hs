module AugsLink.Service.Handlers.RoomWs 
  ( enter
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Network.WebSockets as WS
import Servant
import Data.UUID (fromString, fromText)

import AugsLink.Core.API
import qualified Data.Text as T

type instance Connection IO = WS.PendingConnection

-- Should not terminate until the room is no longer required; because if it
-- does then the ws connection will close on the browser
enter :: Registry IO -> T.Text -> WS.PendingConnection -> Handler ()
enter rr eId pc = liftIO $ do

  let rId = case fromText eId of
              Just roomId -> roomId
              Nothing -> error "Invalid unique id"

  r <- getRoom rr rId

  let room = case r of
               Just rm -> rm
               Nothing -> error "Room does not exist"
  enterRoom room pc

