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
import qualified Data.Heap            as Heap
import qualified Network.WebSockets   as WS

import AugsLink.Core.API
import AugsLink.Core.Shared

type instance Connection IO = WS.PendingConnection
type instance SongFile IO       = MultipartData Mem

type SongQueue = Heap.Heap (Heap.Entry Int Song)

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
      currentlyPlaying  = currentlyPlayingImpl stateVar
    , enqueueSong       = enqueueSongImpl      stateVar
    , enterRoom         = enterRoomImpl        stateVar
    , leaveRoom         = leaveRoomImpl        stateVar
    , presentInRoom     = presentInRoomImpl    stateVar
    , removeSong        = removeSongImpl       stateVar
    , uploadSong        = uploadSongImpl       rId
    }

-- Room API Impls

currentlyPlayingImpl :: MVar RoomState -> IO SongId
currentlyPlayingImpl stateVar = undefined

enqueueSongImpl :: MVar RoomState -> UserId -> SongInfo -> Priority -> IO SongId
enqueueSongImpl stateVar uId sInfo p = do
  sId <- nextRandom
  modifyMVar_ stateVar $ \st -> do
    return $ modQueue st uId (Heap.insert (Heap.Entry p (Song sId sInfo)))
  return sId

enterRoomImpl :: MVar RoomState -> Connection IO -> IO ()
enterRoomImpl stateVar pend = do
  conn <-     WS.acceptRequest pend
  uId <- modifyMVar stateVar $ \st ->
    let spot = Map.size $ roomUsers st 
    in do
      uSt     <-   genNewUser conn spot
      let st' =    addUserToRoom st uSt
          u   =    user uSt
      messageToUser   st' (userId u) (ServerWelcomeMessage u)
      publishToAllBut st' (/= u)     (UserEnterEvent u)
      return  (st', userId u)
  WS.withPingThread conn 30 (return ()) $ handleIncomingMessages stateVar conn uId
  -- todo: deal with async threads
  -- we should keep a reference to the thread so when room is empty we can terminate it 
    
leaveRoomImpl :: MVar RoomState -> UserId -> IO ()
leaveRoomImpl stateVar uId = do
   modifyMVar_ stateVar $ \st -> do
     let st'' = removeUser st uId
     publishToRoom st'' $ UserLeftEvent uId
     return st''
   
   st <- readMVar stateVar
   when (Map.size (roomUsers st) == 0) $
     selfDestruct $ selfManage st 

presentInRoomImpl :: MVar RoomState -> IO [User]
presentInRoomImpl stateVar = do
  roomState <- readMVar stateVar
  return $ map user $ Map.elems $ roomUsers roomState

removeSongImpl :: MVar RoomState -> UserId -> SongId -> IO ()
removeSongImpl stateVar uId sId = do
  modifyMVar_ stateVar $ \st -> do
    return $ modQueue st uId (Heap.filter (not . entryIsSong sId))

uploadSongImpl :: RoomId -> SongId -> SongFile IO -> IO () 
uploadSongImpl rId sId sFile = do
  let file = lookupFile "song" sFile
  either 
    (error "No file present in request") 
    (uploadSongToRoom rId sId) file

-- Messaging Via Websockets

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

-- Song uploading

uploadSongToRoom :: RoomId -> SongId -> FileData Mem -> IO ()
uploadSongToRoom rId sId file = do
  let filePath = "./rooms/" ++ toString rId ++ "/" ++ toString sId
  fileExist <- doesFileExist filePath
  if fileExist 
  then 
   error "Song already uploaded to this room"
  else do
    LBS.writeFile filePath (fdPayload file)

-- User gen

genNewUser :: WS.Connection -> Int -> IO UserState
genNewUser conn spot = do
  uId <- nextRandom
  uName <- return "fisnik"
  return $ UserState conn (User uId uName spot) Heap.empty

-- Pure functions

addUserToRoom :: RoomState -> UserState -> RoomState
addUserToRoom st@(RoomState users _ _) 
              uSt@(UserState _ 
                    (User uId _ _)
                _) = st{roomUsers = Map.insert uId uSt users}

modQueue :: RoomState -> UserId -> (SongQueue -> SongQueue) -> RoomState
modQueue st@(RoomState users _ _) uId f =
   case users Map.! uId of 
     u@(UserState _ _ q) -> 
       st{roomUsers = Map.insert uId (u{userQueue = f q}) users}

removeUser :: RoomState -> UserId -> RoomState
removeUser st@(RoomState users _ _) uId = 
  st'{roomUsers= Map.delete uId (roomUsers st')} 
  where
    st'       = st{roomUsers = Map.map recalcSpot users}
    rmvdUser  = user $ users Map.! uId 
    emptySpot = spotInLine rmvdUser
    recalcSpot uSt = 
       if spot > emptySpot
       then UserState (uStateConn uSt) u{spotInLine=subtract 1 spot} (userQueue uSt)
       else uSt
       where u    = user uSt
             spot = spotInLine u

entryIsSong :: SongId -> Heap.Entry a Song -> Bool
entryIsSong sId (Heap.Entry _ (Song sId' _)) = sId == sId'
