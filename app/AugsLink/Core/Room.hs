module AugsLink.Core.Room
  (
    initialRegistryState
  , initialRoomState
  , newRegistry
  , Registry      (..)
  , RegistryState (..)
  , Room          (..)
  , RoomEvent     (..)
  , RoomId
  , RoomState     (..)
  )
  where

import           Control.Concurrent      (forkIO)
import           Control.Concurrent.Chan (newChan, Chan, readChan, writeChan)
import           Control.Concurrent.MVar (newMVar, readMVar, MVar, modifyMVar_, modifyMVar)
import           Control.Monad           (forM_, when, forever)
import           Data.UUID.V4            (nextRandom)
import qualified Network.WebSockets as WS
import qualified Data.Aeson         as Aeson
import qualified Data.HashMap.Lazy  as HM

import  AugsLink.Core.API
import Data.UUID (toString)

type instance Connection IO = WS.PendingConnection

newtype RegistryState = RegistryState
  {
    rooms :: HM.HashMap RoomId (Room IO)
  }

data RoomState = RoomState
  {
    roomUsers                :: HM.HashMap UserId UserState
  , roomId                   :: RoomId
  , registryChannel          :: Chan InternalMessage
  , songQueue                :: Queue Song
  }

data UserState = UserState
  {
    uStateConn  :: WS.Connection
  , user        :: User
  }

newtype InternalMessage = RoomEmptyMessage RoomId

initialRegistryState :: RegistryState
initialRegistryState = RegistryState 
  {
    rooms=HM.empty
  }

initialRoomState :: RoomId -> Chan InternalMessage -> RoomState
initialRoomState rId rChan = RoomState 
  {
    roomUsers = HM.empty
  , roomId = rId
  , registryChannel = rChan
  }

newRegistry :: IO (Registry IO)
newRegistry = do
  stateVar        <- newMVar initialRegistryState
  registryChannel <- newChan
  _               <- forkIO $ forever $ 
    listen stateVar registryChannel
  return $ Registry
    {
      numRooms =
        HM.size       . rooms <$> readMVar stateVar
    , getRoom = \rId ->
        HM.lookup rId . rooms <$> readMVar stateVar
    , createRoom = do
        rId  <- nextRandom
        room <- newRoom rId registryChannel
        roomCount <- modifyMVar stateVar $ \st -> do
          let rooms' = HM.insert rId room $ rooms st
          return (st{rooms =  rooms'}, HM.size rooms')
        print $ show roomCount ++ " rooms now after creating room " ++ toString rId
        return rId
    , deleteRoom = deleteRoomImpl stateVar
    }

listen :: MVar RegistryState -> Chan InternalMessage -> IO ()
listen stateVar registryChannel = do 
 msg <- readChan   registryChannel
 handleRoomMessage stateVar msg
 listen            stateVar registryChannel

handleRoomMessage :: MVar RegistryState -> InternalMessage -> IO ()
handleRoomMessage stateVar (RoomEmptyMessage rId) = 
  deleteRoomImpl stateVar rId

deleteRoomImpl :: MVar RegistryState -> RoomId -> IO ()
deleteRoomImpl stateVar rId = do
  roomCount <- modifyMVar stateVar $ \st -> do
    let rooms' = HM.delete rId $ rooms st
    return (st{rooms = rooms'}, HM.size rooms')
  print $ show roomCount ++ " rooms left after deleting room " ++ toString rId

newRoom :: RoomId -> Chan InternalMessage -> IO (Room IO)
newRoom rId rrChan = do
  stateVar <- newMVar $ initialRoomState rId rrChan
  return $ Room {
      enterRoom =     enterRoomImpl stateVar
    , leaveRoom =     leaveRoomImpl stateVar
    , presentInRoom = presentInRoomImpl stateVar
    }

presentInRoomImpl :: MVar RoomState -> IO [User]
presentInRoomImpl stateVar = do
  roomState <- readMVar stateVar
  return $ map user $ HM.elems $ roomUsers roomState

enterRoomImpl :: MVar RoomState -> Connection IO -> IO ()
enterRoomImpl stateVar pend = do
  conn <-     WS.acceptRequest pend
  uuid <-     nextRandom
  let uid =   uuid
  modifyMVar_ stateVar $ \st ->
    let spot   = HM.size $ roomUsers st
        u      = User {userId = uid, userName="fisnik", spotInLine=spot}
        uState = UserState {uStateConn=conn, user=u}
        st'    = st{roomUsers = HM.insert uid uState $ roomUsers st}
    in do
      messageToUser   st' uid $ ServerWelcomeMessage $ user uState
      publishToAllBut st' (\us -> us /= user uState) $ UserEnterEvent $ user uState
      return          st'
  WS.withPingThread conn 30 (return ()) $ handleIncomingMessages stateVar conn uid
  -- todo: deal with async threads
  -- we should keep a reference to the thread so when room is empty we can terminate it 

leaveRoomImpl :: MVar RoomState -> UserId -> IO ()
leaveRoomImpl stateVar uid = do
   roomSt' <- modifyMVar stateVar $ \st -> do
     -- modify spots in line
     let users      = roomUsers st
     let emptySpot  = spotInLine $ user $ users HM.! uid
     let st'        = st{roomUsers = HM.map (recalcSpot emptySpot) users}
     let roomUsers' = HM.delete uid $ roomUsers st'
     let st''       = st'{roomUsers = roomUsers'}
     publishToRoom st' $ UserLeftEvent uid
     return (st'', st'')
   when (HM.size (roomUsers roomSt') == 0) $
     sendMessageToRegistry 
       (RoomEmptyMessage $ roomId roomSt') $ 
         registryChannel roomSt'
   where
     recalcSpot :: Int -> UserState -> UserState
     recalcSpot i uSt =
       let u    = user uSt
           spot = spotInLine u
       in if spot > i
          then UserState (uStateConn uSt) u{spotInLine=subtract 1 spot}
          else uSt

sendMessageToRegistry :: InternalMessage -> Chan InternalMessage -> IO ()
sendMessageToRegistry msg rrChan = do writeChan rrChan msg

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
