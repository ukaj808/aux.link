{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Core.User
  (
    newUser
  ) where


import Control.Concurrent
import Data.Text
import Servant.Multipart

import qualified Network.WebSockets as WS

import AugsLink.Core.API

type instance Connection IO = WS.PendingConnection
type instance SongFile IO   = MultipartData Tmp

newtype UserState = UserState
  {
    userData        :: RoomUser
  }

newUser :: RoomId -> UserId -> Bool -> IO (User IO)
newUser rId uId isCreator = do
  let uName = pack $ show uId
  stateVar <- newMVar $ UserState (RoomUser uId uName isCreator)
  return $ User {
    getRoomUser = userData <$> readMVar stateVar
  }