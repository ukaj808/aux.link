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
import           Control.Concurrent.MVar (newMVar, readMVar, MVar, modifyMVar_)
import qualified Data.HashMap.Lazy as HM
import           Data.UUID.V4            (nextRandom)
import qualified Network.WebSockets as WS
import Control.Monad (forM_)

import  AugsLink.Core.API
import AugsLink.Core.Internal
import Data.UUID (toString)

type RoomUserMap = HM.HashMap UserId UserState

newtype RegistryState = RegistryState
  {
     rooms :: HM.HashMap RoomId (Room IO)
  }

data RoomState = RoomState
  {
    roomUsers       :: RoomUserMap
  , roomId          :: RoomId
  }

data UserState = UserState
  {
     uStateConn  :: WS.Connection
  ,  user      :: User
  }

type instance Connection IO = WS.PendingConnection

initialRegistryState :: RegistryState
initialRegistryState = RegistryState {rooms=HM.empty}

initialRoomState :: RoomId -> RoomState
initialRoomState rId = RoomState {roomUsers=HM.empty, roomId=rId}

newRegistry :: IO (Registry IO)
newRegistry = do
  stateVar <- newMVar initialRegistryState
  eventBus <- newEventBus
  threadId <- subscribe eventBus (\(RoomEmptyEvent rId) -> do 
    print "Room Empty!"
    print $ "Deleting Room: " ++ toString rId
    deleteRoomImpl stateVar rId
    )
  return $ Registry
    {
      numRooms =
        HM.size . rooms <$> readMVar stateVar
    , getRoom = \rId ->
        HM.lookup rId . rooms <$> readMVar stateVar
    , createRoom = do
        rId <- nextRandom
        room <- newRoom eventBus rId
        modifyMVar_ stateVar $ \st -> return st{
            rooms =  HM.insert rId room (rooms st)
          }
        return rId
    , deleteRoom = deleteRoomImpl stateVar
    }

deleteRoomImpl :: MVar RegistryState -> RoomId -> IO ()
deleteRoomImpl stateVar rId = do
  modifyMVar_ stateVar $ \st -> return st{
    rooms = HM.delete rId (rooms st)
  }

newRoom :: InternalEventBus -> RoomId -> IO (Room IO)
newRoom eventBus rId = do
  stateVar <- newMVar $ initialRoomState rId
  return $ Room {
      enterRoom = enterRoomImpl stateVar eventBus
    , leaveRoom = leaveRoomImpl stateVar eventBus
    , presentInRoom = presentInRoomImpl stateVar
    }

presentInRoomImpl :: MVar RoomState -> IO [User]
presentInRoomImpl stateVar = do
  roomState <- readMVar stateVar
  let mp = roomUsers roomState
  return $ map user $ HM.elems mp

enterRoomImpl :: MVar RoomState -> InternalEventBus -> Connection IO -> IO ()
enterRoomImpl stateVar eventBus pend = do
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
  WS.withPingThread conn 30 (return ()) (handleIncomingMessages stateVar conn eventBus uid)
  -- todo: deal with async threads
  -- we should keep a reference to the thread so when room is empty we can terminate it 
  --
leaveRoomImpl :: MVar RoomState -> InternalEventBus -> UserId -> IO ()
leaveRoomImpl stateVar eventBus uid = do
   modifyMVar_ stateVar $ \st -> do
     -- modify spots in line
     let users = roomUsers st
     if HM.size users <= 1 
     then do 
       publish eventBus (RoomEmptyEvent (roomId st))
       return st
     else do
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

handleIncomingMessages :: MVar RoomState -> WS.Connection -> InternalEventBus -> UserId -> IO ()
handleIncomingMessages stateVar conn eventBus uid = do
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
          leaveRoomImpl stateVar eventBus uid
        WS.ControlMessage _ -> go
