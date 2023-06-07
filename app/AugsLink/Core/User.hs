{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Core.User
  (
    newUser
  ) where


import Control.Concurrent
import Data.UUID.V4
import Data.UUID

import qualified Data.Heap         as Heap
import qualified Network.WebSockets as WS

import AugsLink.Core.API
import System.Directory (doesFileExist)
import Data.Text
import qualified Data.ByteString.Lazy as LBS
import Servant.Multipart

type instance Connection IO = WS.PendingConnection
type instance SongFile IO       = MultipartData AudioFile
type SongQueue = Heap.Heap (Heap.Entry Int SongId)
data UserState = UserState
  {
    userData        :: RoomUser
  , userQueue       :: SongQueue
  }

newUser :: RoomId -> UserId -> Bool -> IO (User IO)
newUser rId uId isCreator = do
  let uName = pack $ show uId
  stateVar <- newMVar $ UserState (RoomUser uId uName isCreator) Heap.empty
  return $ User {
    getRoomUser = userData <$> readMVar stateVar
  }


modQueue :: UserState -> (SongQueue -> SongQueue) -> UserState
modQueue st@(UserState _ q) f = st{userQueue = f q}

entryIsSong :: SongId -> Heap.Entry a SongId -> Bool
entryIsSong sId (Heap.Entry _ sId') = sId == sId'
