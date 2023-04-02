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
import Data.Text
import Servant.Multipart
import System.Directory

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Lazy    as Map
import qualified Network.WebSockets   as WS

import AugsLink.Core.API
import AugsLink.Core.Shared
import AugsLink.Core.User
import Data.List (sortBy)

type instance Connection IO = WS.PendingConnection
type instance SongFile IO       = MultipartData Mem

data RoomState = RoomState
  {
    roomId                       :: RoomId
  , roomUsers                    :: Map.HashMap UserId UserSession
  , selfManage                   :: SelfManage
  }


data UserSession = USession
  {
    spot :: Int
  , conn :: WS.Connection
  , user :: User IO
  }

initialRoomState :: RoomId -> SelfManage -> RoomState
initialRoomState rId rsm = RoomState 
  {
    roomId     = rId
  , roomUsers  = Map.empty
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
    , viewRoom          = viewRoomImpl    stateVar
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
    rUser   <-     getRoomUser u
    let spot =     Map.size $ roomUsers st
    let st'  =     addUserToRoom st (userId rUser) (USession spot conn u)
    messageToUser   st' (userId rUser) (ServerWelcomeMessage rUser)
    publishToAllBut st' (/= rUser)     (UserEnterEvent rUser)
    return  (st', userId rUser)
  WS.withPingThread conn 30 (return ()) $ handleIncomingMessages stateVar conn uId
  -- todo: deal with async threads
  -- we should keep a reference to the thread so when room is empty we can terminate it 

getUserImpl :: MVar RoomState -> UserId -> IO (Maybe (User IO))
getUserImpl stateVar uId = do
  st <- readMVar stateVar
  return $ user <$> Map.lookup uId (roomUsers st)
    
leaveRoomImpl :: MVar RoomState -> UserId -> IO ()
leaveRoomImpl stateVar uId = do
   modifyMVar_ stateVar $ \st -> do
     let st'' = removeUser st uId
     publishToRoom st'' $ UserLeftEvent uId
     return st''
   
   st <- readMVar stateVar
   when (Map.size (roomUsers st) == 0) $
     selfDestruct $ selfManage st 

viewRoomImpl :: MVar RoomState -> IO [RoomUser]
viewRoomImpl stateVar = do
  roomState <- readMVar stateVar
  users <- mapM (getRoomUser . user) (Map.elems $ roomUsers roomState)
  return $ sortUsers roomState users

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

publishToAllBut :: RoomState -> (RoomUser -> Bool) -> RoomEvent -> IO ()
publishToAllBut rmSt p e = do
  forM_ (roomUsers rmSt) $ \uSession -> do
    rUser <- getRoomUser $ user uSession
    when (p rUser) $ WS.sendTextData (conn uSession) (Aeson.encode e)

publishToRoom ::  RoomState -> RoomEvent -> IO ()
publishToRoom rmSt e = do
  forM_ (roomUsers rmSt) $ \uSession ->
    WS.sendTextData (conn uSession) (Aeson.encode e)

messageToUser :: RoomState -> UserId  -> ServerMessage -> IO ()
messageToUser rmSt uid msg = do
  let uSession = roomUsers rmSt Map.! uid
  WS.sendTextData (conn uSession) (Aeson.encode msg)

-- Song uploading

uploadSongToRoom :: RoomId -> SongId -> FileData Mem -> IO ()
uploadSongToRoom rId sId file = do
  let filePath = "./rooms/" ++ unpack rId ++ "/" ++ unpack sId
  fileExist <- doesFileExist filePath
  if fileExist 
  then 
   error "Song already uploaded to this room"
  else do
    LBS.writeFile filePath (fdPayload file)

-- Pure functions

addUserToRoom :: RoomState -> UserId -> UserSession -> RoomState
addUserToRoom st@(RoomState _ users _) uId uSession = 
  st{roomUsers = Map.insert uId uSession users}

removeUser :: RoomState -> UserId -> RoomState
removeUser st@(RoomState _ users _) uId = 
  st{roomUsers= Map.delete uId users} 

sortUsers :: RoomState -> [RoomUser] -> [RoomUser]
sortUsers st = sortBy (\u u' -> compare (spotInLine u) (spotInLine u'))
  where spotInLine u = spot $ roomUsers st Map.! userId u
