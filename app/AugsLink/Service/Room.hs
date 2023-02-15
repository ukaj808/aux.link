module AugsLink.Service.Room
  (
    newRegistry
  , Room (..)
  , RoomId
  , User (..)
  )
  where

import           Control.Concurrent.MVar (newMVar, readMVar)
import qualified Data.HashMap.Lazy as HM
import           Data.HashMap.Lazy       (HashMap)
import           Control.Concurrent      (modifyMVar_)
import           Data.List               (delete)
import           Data.UUID.V4            (nextRandom)
import           Data.UUID               (UUID)
import qualified Network.WebSockets as WS

import AugsLink.Internal.State ( State(..), get, modify )

type RoomId      =                   UUID
type UserId      =                   String
type Vote        =                   Bool

data Registry m       = Registry
  {
     numRooms       ::                m Int
  ,  createRoom     ::                m (Room m)
  ,  getRoom        ::   RoomId    -> m (Maybe (Room m))
  }

data Room m           = Room
  {
     presentInRoom  ::                m [User]
  ,  enterRoom      ::   User      -> m ()
  ,  leaveRoom      ::   User      -> m ()
  ,  publishToRoom  ::   RoomEvent -> m ()
  }

newtype RegistryState = RegistryState
  {
     rooms          ::                HM.HashMap RoomId (Room IO)
  }


data RoomState        = RoomState
  {
    users           ::                [User]
  , currentSong     ::                Maybe Song
  , vote            ::                [Vote]
  }

data RoomEvent        =               UserEnterEvent User
  |                                   UserLeftEvent User
  |                                   UserVoteEvent User Vote

data User             = User
  {
     conn           ::                WS.Connection
  ,  uid            ::                UserId
  ,  name           ::                String
  ,  queue          ::                [Song]
  }

data Song             = Song
  {
    title           ::                String
  , artist          ::                String
  , duration        ::                Int
  } deriving Show


instance Eq User where
  u == u' = uid u == uid u'

initialRegistryState :: RegistryState
initialRegistryState = RegistryState {rooms=HM.empty}

initialRoomState :: RoomState
initialRoomState = RoomState {users=[], currentSong=Nothing, vote=[]}

modelRegistry :: Registry (State RegistryState)
modelRegistry = Registry
  {
    numRooms = HM.size . rooms <$> get
  , createRoom = undefined
  , getRoom = undefined
  }

modelRoom :: Room (State RoomState)
modelRoom = Room 
  {
    presentInRoom = 
      users <$> get
  , enterRoom = 
      \u -> modify $ \st -> st{users = u:users st}
  , leaveRoom = 
      \u -> modify $ \st -> st{users = delete u (users st)}
  , publishToRoom =
      undefined
  }

newRegistry :: IO (Registry IO)
newRegistry = do
  stateVar <- newMVar initialRegistryState
  return $ Registry
    {
      numRooms = 
        HM.size . rooms <$> readMVar stateVar
    , getRoom = \rId ->
        HM.lookup rId . rooms <$> readMVar stateVar
    , createRoom = do
        room <- newRoom
        rId <- nextRandom
        modifyMVar_ stateVar $ \st -> return st{
            rooms =  HM.insert rId room (rooms st)
          }
        return room
    }

newRoom :: IO (Room IO)
newRoom = do
  stateVar <- newMVar initialRoomState
  return $ Room {
      presentInRoom =
        users <$> readMVar stateVar
    , enterRoom = \u ->
        modifyMVar_ stateVar $ \st -> return st{users = u:users st}
    , leaveRoom = \u ->
        modifyMVar_ stateVar $ \st -> return st{users = delete u (users st)}
    , publishToRoom = 
        undefined
    }
