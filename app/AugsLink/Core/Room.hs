module AugsLink.Core.Room
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

import qualified Data.Aeson as Aeson
import           Control.Concurrent.MVar (newMVar, readMVar, MVar)
import qualified Data.HashMap.Lazy as HM
import           Control.Concurrent      (modifyMVar_, modifyMVar)
import           Data.UUID.V4            (nextRandom)
import qualified Network.WebSockets as WS
import Control.Monad (forM_)

import  AugsLink.Core.API

type RoomUserMap = HM.HashMap UserId UserState

newtype RegistryState = RegistryState
  {
     rooms :: HM.HashMap RoomId (Room IO)
  }

data RoomState = RoomState
  {
    roomUsers       :: RoomUserMap
  }

data UserState = UserState
  {
     uStateConn  :: WS.Connection
  ,  user      :: User
  }

type instance Connection IO = WS.PendingConnection

initialRegistryState :: RegistryState
initialRegistryState = RegistryState {rooms=HM.empty}

initialRoomState :: RoomState
initialRoomState = RoomState {roomUsers=HM.empty}

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
      enterRoom = enterRoomImpl stateVar
    , leaveRoom = leaveRoomImpl stateVar
    , presentInRoom = presentInRoomImpl stateVar
    }

presentInRoomImpl :: MVar RoomState -> IO [User]
presentInRoomImpl stateVar = do
  roomState <- readMVar stateVar
  let mp = roomUsers roomState
  return $ map user $ HM.elems mp

enterRoomImpl :: MVar RoomState -> Connection IO -> IO ()
enterRoomImpl stateVar pend = do
  conn <- WS.acceptRequest pend
  uuid <- nextRandom
  let uid = uuid
  modifyMVar_ stateVar $ \st ->
    let spot   = HM.size $ roomUsers st
        u      = User {userId = uid, userName="fisnik", spotInLine=spot}
        uState = UserState {uStateConn=conn, user=u}
        st'    = st{roomUsers = HM.insert uid uState $ roomUsers st}
    in do
      messageToUser st' uid (ServerWelcomeMessage $ user uState)
      publishToAllBut st' (\us -> us /= user uState) (UserEnterEvent $ user uState)
      return st'
  WS.withPingThread conn 30 (return ()) (handleIncomingMessages stateVar conn uid)
  -- todo: deal with async threads
  -- we should keep a reference to the thread so when room is empty we can terminate it 
  --
leaveRoomImpl :: MVar RoomState -> UserId -> IO ()
leaveRoomImpl stateVar uid = do
   modifyMVar_ stateVar $ \st -> do
     -- modify spots in line
     let users = roomUsers st
     let emptySpot = spotInLine $ user $ users HM.! uid
     let st' = st{roomUsers = HM.map (recalcSpot emptySpot) users}
     publishToRoom st' $ UserLeftEvent uid
     return st'{roomUsers = HM.delete uid (roomUsers st')}
   where
     recalcSpot :: Int -> UserState -> UserState
     recalcSpot i uSt = 
       let u    = user uSt
           spot = spotInLine u
       in  if spot > i 
           then UserState (uStateConn uSt) u{spotInLine=subtract 1 spot}
           else uSt
           
publishToAllBut :: RoomState -> (User -> Bool) -> RoomEvent -> IO ()
publishToAllBut rmSt p e = do
  forM_ (HM.filter (p . user) (roomUsers rmSt)) $ \u ->
    WS.sendTextData (uStateConn u) (Aeson.encode e)

publishToRoom ::  RoomState -> RoomEvent -> IO ()
publishToRoom rmSt e = do
  forM_ (roomUsers rmSt) $ \u ->
    WS.sendTextData (uStateConn u) (Aeson.encode e)

messageToUser :: RoomState -> UserId  -> ServerMessage -> IO ()
messageToUser rmSt uid msg = do
  let u = roomUsers rmSt HM.! uid
  WS.sendTextData (uStateConn u) (Aeson.encode msg)

handleIncomingMessages :: MVar RoomState -> WS.Connection -> UserId -> IO ()
handleIncomingMessages stateVar conn uid = do
  go
  where
    go :: IO ()
    go  = do
      msg <- WS.receive conn
      case msg of
        WS.DataMessage {} -> do
          print "Should not be possible"
          go
        WS.ControlMessage WS.Close {} -> do
          leaveRoomImpl stateVar uid
        WS.ControlMessage _ -> go
