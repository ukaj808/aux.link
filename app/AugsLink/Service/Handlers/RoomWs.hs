module AugsLink.Service.Handlers.RoomWs 
  ( join
  ) where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import qualified Network.WebSockets as WS
import Network.WebSockets.Connection (acceptRequest, withPingThread)
import Servant

import AugsLink.Service.Room (enterRoom, User (..), RoomControl (presentInRoom))
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import qualified Data.Text as T

publish :: String -> [User] -> IO ()
publish event users = do
  putStrLn event
  mapM_ (\c -> WS.sendTextData c (T.pack event)) connections
    where
    connections = map conn users

join :: RoomControl IO -> String -> WS.PendingConnection -> Handler ()
join rc rId pc = liftIO $ do

  conn <- liftIO $ acceptRequest pc

  uuid <- nextRandom 

  let uid = toString uuid

  let user = User conn uid uid []

  enterRoom rc user rId

  withPingThread conn 30 
    (print "ping") 
      (
      forever $ do
        WS.sendTextData conn  (T.pack "Welcome to the room")
        users <- presentInRoom rc rId 
        publish "New user has joined" users
      )
