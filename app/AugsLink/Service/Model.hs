module AugsLink.Service.Model where

import Data.List (delete)
import qualified Data.UUID as UUID

import AugsLink.Service.Room
  (
     RegistryState (..)
  ,  Registry (..)
  ,  RoomState (..)
  ,  Room (..)
  ,  RoomId, initialRoomState
  )
import AugsLink.Internal.State
  (
     State (State)
  ,  modify
  ,  get
  )

import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M
import Control.Monad.State (MonadState(state))
import Data.ByteString
import Data.UUID

type InternalId = Int

data Model = Model {
   modelRooms   ::   M.Map Int RoomState
 , modelRoomIds ::   HM.HashMap RoomId InternalId
 , modelNextId  ::   InternalId
 }

modelRegistry :: Registry (State Model)
modelRegistry = Registry
  {
    numRooms = M.size . modelRooms <$> get
  , createRoom = State $ \st ->
      let iId = modelNextId st
          rId = UUID.fromWords 0 0 0 (fromIntegral iId)
      in ( rId
         , st { modelRooms  = M.insert iId initialRoomState (modelRooms st)
              , modelNextId = succ iId
              , modelRoomIds = HM.insert rId iId (modelRoomIds st)
              }
         )
  , getRoom = undefined
  }

modelRoom :: Room (State Model)
modelRoom = Room
  {
    presentInRoom = undefined
  , enterRoom = undefined
  , leaveRoom = undefined 
  , publishToRoom = undefined
  }

