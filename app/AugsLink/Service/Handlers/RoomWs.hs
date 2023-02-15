module AugsLink.Service.Handlers.RoomWs 
  ( enter
  ) where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import qualified Network.WebSockets as WS
import Network.WebSockets.Connection (acceptRequest, withPingThread)
import Servant

import AugsLink.Service.Room (enterRoom, User (..), Registry (getRoom), Room (..), RoomEvent (..) )
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString, fromString)
import qualified Data.Text as T

publish :: String -> [User] -> IO ()
publish event users = do
  putStrLn event
  mapM_ (\c -> WS.sendTextData c (T.pack event)) connections
    where
    connections = map conn users

enter :: Registry IO -> String -> WS.PendingConnection -> Handler ()
enter rr eId pc = liftIO $ do

  conn <- liftIO $ acceptRequest pc

  uuid <- nextRandom 

  let uid = toString uuid

  let user = User conn uid uid []

  let rId = case fromString eId of
              Just roomId -> roomId
              Nothing -> error "Invalid unique id"

  r <- getRoom rr rId

  let room = case r of
               Just rm -> rm
               Nothing -> error "Room does not exist"

  enterRoom room user

  -- publishToRoom room (UserEnterEvent user)

  withPingThread conn 30 
    (print "ping") 
      (
      forever $ do
        WS.sendTextData conn  (T.pack "Welcome to the room")
        users <- presentInRoom room
        publish "New user has joined" users
      )
