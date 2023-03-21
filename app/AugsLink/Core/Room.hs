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
import Servant.Multipart
import System.Directory

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Lazy    as Map
import qualified Network.WebSockets   as WS

import AugsLink.Core.API
import AugsLink.Core.Shared
import AugsLink.Core.User

type instance Connection IO = WS.PendingConnection
type instance SongFile IO       = MultipartData Mem

data RoomState = RoomState
  {
    roomId                       :: RoomId
  , roomUsers                    :: Map.HashMap UserId (WS.Connection, User IO)
  , roomOrder                    :: [UserId]
  , selfManage                   :: SelfManage
  }

initialRoomState :: RoomId -> SelfManage -> RoomState
initialRoomState rId rsm = RoomState 
  {
    roomId     = rId
  , roomUsers  = Map.empty
  , roomOrder  = []
  , selfManage = rsm
  }

newRoom :: RoomId -> SelfManage -> IO (Room IO)
newRoom rId selfManage = do
  stateVar <- newMVar $ initialRoomState rId selfManage
  return $ Room {
      currentlyPlaying  = currentlyPlayingImpl stateVar
    , enterRoom         = enterRoomImpl        stateVar
    , getUser           = getUserImpl          stateVar
    , leaveRoom         = leaveRoomImpl        stateVar
    , presentInRoom     = presentInRoomImpl    stateVar
    , uploadSong        = uploadSongImpl       rId
    }

-- Room API Impls
currentlyPlayingImpl :: MVar RoomState -> IO SongId
currentlyPlayingImpl stateVar = undefined

enterRoomImpl :: MVar RoomState -> Connection IO -> IO ()
enterRoomImpl stateVar pend = do
  conn <-     WS.acceptRequest pend
  uId <- modifyMVar stateVar $ \st -> do
    u       <-     newUser
    uData   <-     getUserData u
    let st' =      addUserToRoom st (userId uData, conn, u)
    messageToUser   st' (userId uData) (ServerWelcomeMessage uData)
    publishToAllBut st' (/= uData)     (UserEnterEvent uData)
    return  (st', userId uData)
  WS.withPingThread conn 30 (return ()) $ handleIncomingMessages stateVar conn uId
  -- todo: deal with async threads
  -- we should keep a reference to the thread so when room is empty we can terminate it 

getUserImpl :: MVar RoomState -> UserId -> IO (Maybe (User IO))
getUserImpl stateVar uId = do
  st <- readMVar stateVar
  return $ snd <$> Map.lookup uId (roomUsers st)
    
leaveRoomImpl :: MVar RoomState -> UserId -> IO ()
leaveRoomImpl stateVar uId = do
   modifyMVar_ stateVar $ \st -> do
     let st'' = removeUser st uId
     publishToRoom st'' $ UserLeftEvent uId
     return st''
   
   st <- readMVar stateVar
   when (Map.size (roomUsers st) == 0) $
     selfDestruct $ selfManage st 

presentInRoomImpl :: MVar RoomState -> IO [User IO]
presentInRoomImpl stateVar = do
  roomState <- readMVar stateVar
  return $ map snd $ Map.elems $ roomUsers roomState

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

publishToAllBut :: RoomState -> (UserData -> Bool) -> RoomEvent -> IO ()
publishToAllBut rmSt p e = do
  forM_ (roomUsers rmSt) $ \(conn, u) -> do
    uData <- getUserData u
    when (p uData) $ WS.sendTextData conn (Aeson.encode e)

publishToRoom ::  RoomState -> RoomEvent -> IO ()
publishToRoom rmSt e = do
  forM_ (roomUsers rmSt) $ \(conn, _) ->
    WS.sendTextData conn (Aeson.encode e)

messageToUser :: RoomState -> UserId  -> ServerMessage -> IO ()
messageToUser rmSt uid msg = do
  let (conn, _) = roomUsers rmSt Map.! uid
  WS.sendTextData conn $ Aeson.encode msg

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

-- Pure functions

addUserToRoom :: RoomState -> (UserId, WS.Connection, User IO) -> RoomState
addUserToRoom st@(RoomState _ users order _) (uId, conn, uIo) = st{roomUsers = Map.insert uId (conn, uIo) users, roomOrder = order ++ [uId]}

removeUser :: RoomState -> UserId -> RoomState
removeUser st@(RoomState _ users order _) uId = 
  st{roomUsers= Map.delete uId users, roomOrder= filter (/= uId) order} 
