module AugsLink.Service.Room
  (
    RoomServer
  , RoomControl (..)
  , newRoomServer
  , createRoom
  , enterRoom
  , User (..)
  , RoomId
  )
  where

import Control.Concurrent.MVar (newMVar, readMVar, takeMVar)
import Data.HashMap.Lazy ((!))
import qualified Data.HashMap.Lazy as HM
import System.Random

import AugsLink.Internal.State ( State(..), get, modify )
import Control.Concurrent (putMVar, modifyMVar)
import Data.Char (isAlphaNum)
import qualified Network.WebSockets as WS

type RoomServer = IO (RoomControl IO)
type ModelServer = RoomControl (State RoomControlState)

data RoomControl m = RControl
  {
     numRooms ::                         m Int
  ,  presentInRoom  :: RoomId              ->  m [User]
  ,  createRoom   ::                         m RoomId
  ,  enterRoom    :: User      -> RoomId ->  m ()
  ,  leaveRoom    :: User      -> RoomId ->  m ()
  }

type RoomId = String
type UserId = String

data User = User
  {
     conn :: WS.Connection
  ,  uid   :: UserId
  ,  name :: String
  ,  queue :: [Song]
  }

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
  }

newtype RoomControlState = RControlState
 {
   rooms :: HM.HashMap RoomId Room
 }

data RoomEvent =
    UserEnterEvent User
  | UserLeftEvent User
  | UserVoteEvent User Vote

initialControlState :: RoomControlState
initialControlState = RControlState HM.empty

emptyRoom :: Room
emptyRoom = Room [] Nothing []

genRoomId :: IO RoomId
genRoomId = do
  gen <- newStdGen
  let chars = randomRs ('0', 'z') gen
  return $ take 8 (filter isAlphaNum chars)

modelServer :: ModelServer
modelServer = RControl
  {
    numRooms = HM.size . rooms <$> get

  , presentInRoom = \rId -> (\s -> users (rooms s ! rId)) <$> get

  , createRoom = undefined

  , enterRoom = \u rId -> modify $ \s ->
      let room = (rooms s ! rId)
          room' = Room (u : users room) (currentSong room) (vote room)
      in RControlState $ HM.insert rId room' (rooms s)

  , leaveRoom = \u rId -> modify $ \s ->
      let room = (rooms s ! rId)
          room' = Room (filter (u /=) (users room)) (currentSong room) (vote room)
      in RControlState $ HM.insert rId room' (rooms s)

  }

newRoomServer :: RoomServer
newRoomServer = do
  stateVar <- newMVar initialControlState
  return $ RControl
    {
      numRooms = HM.size . rooms <$> readMVar stateVar
    , presentInRoom = \rId -> do
        roomServer <- readMVar stateVar
        return $ users $ rooms roomServer ! rId

    , createRoom = do -- todo: retry mechanism
        modifyMVar stateVar
          (\rs -> do
            rId <- genRoomId
            let rms = rooms rs
            putStrLn "wahtt"
            let roomServer' = if HM.member rId rms
                              then RControlState $ HM.insert rId emptyRoom rms
                              else error "Room id already exists"
            return (roomServer', rId))

    , enterRoom = \u rId -> do
        roomServer <- takeMVar stateVar
        let rs = rooms roomServer
        let room = rs ! rId
        let room' = Room (u : users room) (currentSong room) (vote room)
        let roomServer' = RControlState $ HM.insert rId room' rs
        putMVar stateVar roomServer'

    , leaveRoom = \u rId -> do
        roomServer <- takeMVar stateVar
        let rs = rooms roomServer
        let room = rs ! rId
        let room' = Room (filter (u /=) (users room)) (currentSong room) (vote room)
        let roomServer' = RControlState $ HM.insert rId room' rs
        putMVar stateVar roomServer'
    }
