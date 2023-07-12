module AugsLink.Core.User
  (
    newUser
  ) where


import Control.Concurrent
import Data.Text

import qualified Network.WebSockets as WS

import AugsLink.Core.API

type instance Connection IO = WS.PendingConnection

newtype UserState = UserState
  {
    userData        :: RoomUser
  }

newUser :: RoomId -> UserId -> IO (User IO)
newUser rId uId = do
  let uName = pack $ show uId
  stateVar <- newMVar $ UserState (RoomUser uId uName)
  return $ User {
    getRoomUser = userData <$> readMVar stateVar
  }