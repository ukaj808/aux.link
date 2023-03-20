{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Core.Room
  (
    initialRoomState
  , Room          (..)
  , RoomState     (..)
  , newRoom
  )
  where

import Control.Concurrent.MVar
import Control.Monad
import Data.UUID
import Data.UUID.V4 
import Servant.Multipart
import System.Directory

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Lazy    as Map
import qualified Network.WebSockets   as WS

import Commons.Queue
import AugsLink.Core.API
import AugsLink.Core.Shared

type instance Connection IO = WS.PendingConnection
type instance SongFile IO       = MultipartData Mem

type SongQueue = BatchedQueue Song

data RoomState = RoomState
  {
    roomUsers                :: Map.HashMap UserId UserState
  , roomId                   :: RoomId
  , selfManage               :: SelfManage
  }

data UserState = UserState
  {
    uStateConn  :: WS.Connection
  , user        :: User
  , userQueue   :: SongQueue
  }

initialRoomState :: RoomId -> SelfManage -> RoomState
initialRoomState rId rsm = RoomState 
  {
    roomUsers  = Map.empty
  , roomId     = rId
  , selfManage = rsm 
  }

newRoom :: RoomId -> SelfManage -> IO (Room IO)
newRoom rId selfManage = do
  stateVar <- newMVar $ initialRoomState rId selfManage
  return $ Room {
      enterRoom         = enterRoomImpl        stateVar
    , leaveRoom         = leaveRoomImpl        stateVar
    , presentInRoom     = presentInRoomImpl    stateVar
    , currentlyPlaying  = currentlyPlayingImpl stateVar
    , enqueueSong       = enqueueSongImpl      stateVar
    , uploadSong        = uploadSongImpl       rId
    , modifyQueueOrder  = modifyQueueOrderImpl stateVar
    , removeSong        = removeSongImpl       stateVar
    }

removeSongImpl :: MVar RoomState -> UserId -> SongId -> IO ()
removeSongImpl stateVar uId sId = do
  modifyMVar_ stateVar $ \st -> do
    let uState  = roomUsers st Map.! uId
    let q'      = qremove (userQueue uState) sId
    let uState' = uState{userQueue = q'}
    let st'     = st{roomUsers = Map.insert uId uState' $ roomUsers st}
    return st'

enqueueSongImpl :: MVar RoomState -> UserId -> SongInfo -> IO SongId
enqueueSongImpl stateVar uId sInfo = do
  sId <- nextRandom
  modifyMVar_ stateVar $ \st -> do
    let uState  = roomUsers st Map.! uId
    let q'      = enqueue (userQueue uState) (Song sId sInfo)
    let uState' = uState{userQueue = q'}
    let st'     = st{roomUsers = Map.insert uId uState' $ roomUsers st}
    return st'
  return sId

modifyQueueOrderImpl :: MVar RoomState -> UserId -> [SongId] -> IO ()
modifyQueueOrderImpl stateVar uId newOrder = do
  modifyMVar_ stateVar $ \st -> do
    let uState  = roomUsers st Map.! uId
    let q'      = reorder (userQueue uState) newOrder
    let uState' = uState{userQueue = q'}
    let st'     = st{roomUsers = Map.insert uId uState' $ roomUsers st}
    return st'


uploadSongImpl :: RoomId -> SongId -> SongFile IO -> IO () 
uploadSongImpl rId sId sFile = do
  let file = lookupFile "song" sFile
  either 
    (error "No file present in request") 
    (uploadSongToRoom rId sId) file

uploadSongToRoom :: RoomId -> SongId -> FileData Mem -> IO ()
uploadSongToRoom rId sId file = do
  let filePath = "./rooms/" ++ toString rId ++ "/" ++ toString sId
  fileExist <- doesFileExist filePath
  if fileExist 
  then 
   error "Song already uploaded to this room"
  else do
    LBS.writeFile filePath (fdPayload file)

currentlyPlayingImpl :: MVar RoomState -> IO SongId
currentlyPlayingImpl stateVar = undefined

presentInRoomImpl :: MVar RoomState -> IO [User]
presentInRoomImpl stateVar = do
  roomState <- readMVar stateVar
  return $ map user $ Map.elems $ roomUsers roomState

enterRoomImpl :: MVar RoomState -> Connection IO -> IO ()
enterRoomImpl stateVar pend = do
  conn <-     WS.acceptRequest pend
  uuid <-     nextRandom
  let uid =   uuid
  modifyMVar_ stateVar $ \st ->
    let spot   = Map.size $ roomUsers st
        u      = User {userId = uid, userName="fisnik", spotInLine=spot}
        uState = UserState {uStateConn=conn, user=u, userQueue=qempty}
        st'    = st{roomUsers = Map.insert uid uState $ roomUsers st}
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
     let emptySpot  = spotInLine $ user $ users Map.! uid
     let st'        = st{roomUsers = Map.map (recalcSpot emptySpot) users}
     let roomUsers' = Map.delete uid $ roomUsers st'
     let st''       = st'{roomUsers = roomUsers'}
     publishToRoom st' $ UserLeftEvent uid
     return (st'', st'')
   when (Map.size (roomUsers roomSt') == 0) $
     selfDestruct $ selfManage roomSt' 
   where
     recalcSpot :: Int -> UserState -> UserState
     recalcSpot i uSt =
       let u    = user uSt
           spot = spotInLine u
       in if spot > i
          then UserState (uStateConn uSt) u{spotInLine=subtract 1 spot} (userQueue uSt)
          else uSt

publishToAllBut :: RoomState -> (User -> Bool) -> RoomEvent -> IO ()
publishToAllBut rmSt p e = do
  forM_ (Map.filter (p . user) (roomUsers rmSt)) $ \u ->
    WS.sendTextData (uStateConn u) (Aeson.encode e)

publishToRoom ::  RoomState -> RoomEvent -> IO ()
publishToRoom rmSt e = do
  forM_ (roomUsers rmSt) $ \u ->
    WS.sendTextData (uStateConn u) (Aeson.encode e)

messageToUser :: RoomState -> UserId  -> ServerMessage -> IO ()
messageToUser rmSt uid msg = do
  let u = roomUsers rmSt Map.! uid
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
          putStrLn "Should not be possible"
          go
        WS.ControlMessage WS.Close {} -> do
          leaveRoomImpl stateVar uid
        WS.ControlMessage _ -> go
