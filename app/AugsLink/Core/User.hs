module AugsLink.Core.User
  (
    newUser
  ) where


import Control.Concurrent

import qualified Network.WebSockets as WS

import AugsLink.Core.API
import Data.UUID.V4
import Data.UUID

type instance Connection IO = WS.PendingConnection

newtype UserState = UserState
  {
    userData        :: RoomUser
  }

newUser :: IO (User IO)
newUser = do
  uuid <- toText <$> nextRandom
  stateVar <- newMVar $ UserState (RoomUser uuid uuid)
  return $ User {
    getRoomUser = userData <$> readMVar stateVar
  }