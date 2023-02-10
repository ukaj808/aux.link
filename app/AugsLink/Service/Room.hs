module AugsLink.Service.Room where

import Control.Concurrent.MVar (newMVar, readMVar, takeMVar)
import Data.HashMap.Lazy ((!))
import qualified Data.HashMap.Lazy as HM
import System.Random

import AugsLink.Internal.State ( State(..), get, modify )
import Control.Concurrent (putMVar)
import Data.Char (isAlphaNum)


data RoomServer m = RServer
  {
     numRooms ::                         m Int
  ,  presentInRoom  :: RoomId              ->  m [User]
  ,  createRoom   ::                         m RoomId
  ,  enterRoom    :: User      -> RoomId ->  m ()
  ,  leaveRoom    :: User      -> RoomId ->  m ()
  ,  publishToRoom  :: RoomEvent -> RoomId ->  m ()
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
  , currentSong :: Maybe Song
  , vote        :: [Vote]
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

emptyRoom :: Room
emptyRoom = Room [] Nothing []

genRoomId :: IO RoomId
genRoomId = do
  gen <- newStdGen
  let chars = randomRs ('0', 'z') gen
  return $ take 8 (filter isAlphaNum chars)

modelServerState :: RoomServer (State RoomServerState)
modelServerState = RServer 
  { 
    numRooms = HM.size . rooms <$> get

  , presentInRoom = \rId -> (\s -> users (rooms s ! rId)) <$> get
    
  , createRoom = undefined

  , enterRoom = \u rId -> modify $ \s -> 
      let room = (rooms s ! rId)
          room' = Room (u : users room) (currentSong room) (vote room)
      in RServerState $ HM.insert rId room' (rooms s)

  , leaveRoom = \u rId -> modify $ \s -> 
      let room = (rooms s ! rId)
          room' = Room (filter (u /=) (users room)) (currentSong room) (vote room)
      in RServerState $ HM.insert rId room' (rooms s)

  , publishToRoom = undefined
  }

newRoomServer :: IO (RoomServer IO)
newRoomServer = do
  stateVar <- newMVar initialServerState
  return $ RServer 
    {
      numRooms = HM.size . rooms <$> readMVar stateVar
    , presentInRoom = \rId -> do
        roomServer <- readMVar stateVar
        return $ users $ rooms roomServer ! rId
        
    , createRoom = do -- todo: retry mechanism
        roomServer <- takeMVar stateVar
        rId <- genRoomId
        let rs = rooms roomServer
        let roomServer' = if HM.member rId rs 
                          then RServerState $ HM.insert rId emptyRoom rs 
                          else error "Room id already exists"
        putMVar stateVar roomServer'
        return rId

    , enterRoom = \u rId -> do
        roomServer <- takeMVar stateVar
        let rs = rooms roomServer
        let room = rs ! rId
        let room' = Room (u : users room) (currentSong room) (vote room)
        let roomServer' = RServerState $ HM.insert rId room' rs 
        putMVar stateVar roomServer' 

    , leaveRoom = \u rId -> do
        roomServer <- takeMVar stateVar
        let rs = rooms roomServer
        let room = rs ! rId
        let room' = Room (filter (u /=) (users room)) (currentSong room) (vote room)
        let roomServer' = RServerState $ HM.insert rId room' rs
        putMVar stateVar roomServer'

    , publishToRoom = undefined
    }
