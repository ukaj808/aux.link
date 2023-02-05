module AugsLink.Service.Room where

import Control.Concurrent.MVar (newMVar, readMVar)
import Data.HashMap.Lazy ((!))
import qualified Data.HashMap.Lazy as HM

import AugsLink.Internal.State ( State(..), get, modify )


data RoomServer m = RServer
  {
     numRooms ::                         m Int
  ,  present  :: RoomId              ->  m [User]
  ,  enter    :: User      -> RoomId ->  m ()
  ,  leave    :: User      -> RoomId ->  m ()
  ,  publish  :: RoomEvent -> RoomId ->  m ()
  }

type RoomId = String
type UserId = String

data User = User
  {
     uid   :: UserId
  ,  name :: String
  ,  queue :: [Song]
  } deriving Show

instance Eq User where
  u == u' = uid u == uid u'

data Song = Song
  {
    title :: String
  , artist :: String
  , length :: Int
  } deriving Show

type Vote = Bool

data Room = Room 
  {
    users :: [User]
  , currentSong :: Song
  , vote        :: Vote
  } deriving Show

newtype RoomServerState = RServerState
 {
   rooms :: HM.HashMap RoomId Room
 }

data RoomEvent = 
    UserEnterEvent User 
  | UserLeftEvent User 
  | UserVoteEvent User Vote

initialServerState :: RoomServerState
initialServerState = RServerState HM.empty

modelServerState :: RoomServer (State RoomServerState)
modelServerState = RServer 
  { 
    numRooms = HM.size . rooms <$> get

  , present = \rId -> (\s -> users (rooms s ! rId)) <$> get

  , enter = \u rId -> modify $ \s -> 
      let room = (rooms s ! rId)
          room' = Room (u : users room) (currentSong room) (vote room)
      in RServerState $ HM.insert rId room' (rooms s)

  , leave = \u rId -> modify $ \s -> 
      let room = (rooms s ! rId)
          room' = Room (filter (u /=) (users room)) (currentSong room) (vote room)
      in RServerState $ HM.insert rId room' (rooms s)

  , publish = undefined
  }

newServer :: IO (RoomServer IO)
newServer = do
  stateVar <- newMVar initialServerState
  return $ RServer 
    {
      numRooms = HM.size . rooms <$> readMVar stateVar
    , present = undefined
    , enter = undefined
    , leave = undefined
    , publish = undefined
    }


