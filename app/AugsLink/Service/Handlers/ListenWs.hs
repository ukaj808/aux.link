module AugsLink.Service.Handlers.ListenWs
  ( 
    listenHandler
  ) where

import Control.Monad.IO.Class
import Data.Text
import Servant

import qualified Network.WebSockets as WS

import AugsLink.Core.API hiding (listen)

type instance Connection IO = WS.PendingConnection

-- Should not terminate until the room is no longer required; because if it
-- does then the ws connection will close on the browser
listenHandler :: Registry IO -> Text ->  Text -> WS.PendingConnection -> Handler ()
listenHandler rr rId uId pc = liftIO $ do

  r <- getRoom rr rId

  let room = case r of
               Just rm -> rm
               Nothing -> error "Room does not exist"

  u <- getUser room uId
  let user = case u of
               Just us -> us
               Nothing -> error "User does not exist"
  
  listenToMusic user pc