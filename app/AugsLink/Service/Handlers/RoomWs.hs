module AugsLink.Service.Handlers.RoomWs 
  ( join
  ) where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Network.WebSockets (PendingConnection)
import Network.WebSockets.Connection (acceptRequest, withPingThread)
import Servant

-- copy job; need to understand more
join :: String -> PendingConnection -> Handler ()
 -- Join a Room kept in memory returning an accepted websocket connection
join id pc = liftIO $ do
  conn <- liftIO $ acceptRequest pc
  withPingThread conn 30 postPingAction (forever interactWithRoom)
  where 
    postPingAction :: IO ()
    postPingAction = print "ping"

    interactWithRoom :: IO ()
    interactWithRoom = do
      print "hey"

