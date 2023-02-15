{-# LANGUAGE OverloadedStrings #-}

module AugsLink.Service.Room
  (
    initialRegistryState 
  , initialRoomState
  , newRegistry
  , Registry (..)
  , RegistryState (..)
  , RoomState (..)
  , RoomEvent (..)
  , Room (..)
  , RoomId
  )
  where

import           Control.Concurrent.MVar (newMVar, readMVar, MVar)
import qualified Data.HashMap.Lazy as HM
import           Control.Concurrent      (modifyMVar_)
import           Data.UUID.V4            (nextRandom)
import           Data.UUID               (UUID, toString)
import qualified Network.WebSockets as WS
import Control.Monad (forM_)
import Data.Aeson (FromJSON, Value, ToJSON (..), KeyValue ((.=)), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson.Types (Parser)

type RoomId      =                   UUID
type UserId      =                   String --todo uuid
type Vote        =                   Bool
type Username    =                   String

data Registry m       = Registry
  {
     numRooms       ::                m Int
  ,  createRoom     ::                m RoomId
  ,  getRoom        ::   RoomId    -> m (Maybe (Room m))
  }

data Room m           = Room
  {
     presentInRoom  ::                m [UserId]
  ,  enterRoom      ::   Connection m -> m ()
  ,  leaveRoom      ::   UserId      -> m ()
  ,  publishToRoom  ::   RoomEvent -> m ()
  }

newtype RegistryState = RegistryState
  {
     rooms          ::                HM.HashMap RoomId (Room IO)
  }

data RoomState        = RoomState
  {
    roomUsers           ::                HM.HashMap UserId UserState               
  , roomCurrentSong     ::                Maybe Song
  , roomVote            ::                [Vote]
  }

data UserState        = UserState
  {
     userConn           ::                WS.Connection
  ,  userQueue          ::                [Song]
  ,  userOrder          ::                Int
  ,  userName           ::                Username
  }

data RoomEvent        =               UserEnterEvent UserId Username
  |                                   UserLeftEvent UserId
  |                                   UserVoteEvent UserId Vote

instance ToJSON RoomEvent where
  toJSON :: RoomEvent -> Value
  toJSON (UserEnterEvent uid uname) = Aeson.object 
    [
       "type"      .= ("UserEnterEvent" :: String)
    ,  "userId"    .= uid
    ,  "username"  .= uname
    ]
  
instance FromJSON RoomEvent where
  parseJSON :: Value -> Parser RoomEvent
  parseJSON = Aeson.withObject "RoomEvent" $ \obj -> do
      typ <- obj .: "type"
      case typ :: String of
        "UserEnterEvent" -> do
          userId <- obj .: "userId"
          userName <- obj .: "username"
          return $ UserEnterEvent userId userName
          

data Song             = Song
  {
    title           ::                String
  , artist          ::                String
  , duration        ::                Int
  } deriving (Generic, FromJSON, ToJSON, Show)


type family Connection (m :: * -> *) :: *

type instance Connection IO = WS.PendingConnection

initialRegistryState :: RegistryState
initialRegistryState = RegistryState {rooms=HM.empty}

initialRoomState :: RoomState
initialRoomState = RoomState {roomUsers=HM.empty, roomCurrentSong=Nothing, roomVote=[]}

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
        return rId
    }

newRoom :: IO (Room IO)
newRoom = do
  stateVar <- newMVar initialRoomState
  return $ Room {
      presentInRoom =
        HM.keys . roomUsers <$> readMVar stateVar

    , enterRoom = enterRoomImpl stateVar
    , leaveRoom = \u ->
        modifyMVar_ stateVar $ \st -> return st{roomUsers = HM.delete u (roomUsers st)}
    , publishToRoom = publishToRoomImpl stateVar
    }

enterRoomImpl :: MVar RoomState -> Connection IO -> IO ()
enterRoomImpl stateVar pend = do
       
    conn <- WS.acceptRequest pend
    putStrLn "accepted connection"
    uuid <- nextRandom 
    let uid = toString uuid
    let user = UserState {userConn=conn, userQueue=[], userOrder=1, userName=uid  } 
    publishToRoomImpl stateVar $ UserEnterEvent uid (userName user)
    modifyMVar_ stateVar $ \st -> return st{roomUsers = HM.insert uid user (roomUsers st)}
    WS.withPingThread conn 30 (return ()) (handleIncomingEvents stateVar conn)
    -- todo: deal with async threads
    -- we should keep a reference to the thread so when room is empty we can terminate it 

publishToRoomImpl :: MVar RoomState -> RoomEvent -> IO ()
publishToRoomImpl stateVar e = do
  rmSt <- readMVar stateVar
  putStrLn $ "broadcasting event: " ++ (show $ Aeson.encode e) ++ "# connections: " ++ show (length $ roomUsers rmSt)
  forM_ (roomUsers rmSt) $ \u ->
    WS.sendTextData (userConn u) (Aeson.encode e)
  
handleIncomingEvents :: MVar RoomState -> WS.Connection -> IO ()
handleIncomingEvents stateVar conn = do
  putStrLn "Start handle events"
  go
  where
    go :: IO () 
    go  = do 
      msg <- WS.receiveData conn
      print msg
      case Aeson.eitherDecode msg of
        Left e -> do 
          print e
          WS.sendClose conn $ T.pack e
        Right event -> do 
          publishToRoomImpl stateVar event
          go
