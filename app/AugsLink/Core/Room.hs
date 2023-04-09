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
import Data.List
import Data.Text
import Servant.Multipart
import System.Directory
import Data.UUID
import Data.UUID.V4

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map    as Map
import qualified Network.WebSockets   as WS

import AugsLink.Core.API
import AugsLink.Core.Music
import AugsLink.Core.Shared
import AugsLink.Core.User

type instance Connection IO = WS.PendingConnection
type instance SongFile IO       = MultipartData Mem

data RoomState = RoomState
  {
    roomId                       :: RoomId
  , roomUsers                    :: Map.Map UserId UserSession
  , registryManage               :: RegistryManage
  , music                        :: Music IO
  , creator                      :: Maybe UserId
  , nextPos                      :: Int
  , userCount                    :: Int
  }


data UserSession = USession
  {
    conn :: WS.Connection
  , user :: User IO
  }

initialRoomState :: RoomId -> RegistryManage -> Music IO -> RoomState
initialRoomState rId rsm music = RoomState
  {
    roomId         = rId
  , roomUsers      = Map.empty
  , registryManage = rsm
  , music          = music
  , creator        = Nothing
  , nextPos     = 0
  , userCount      = 0
  }

newRoom :: RoomId -> RegistryManage -> IO (Room IO)
newRoom rId registryManage = do
  music    <- newMusic
  stateVar <- newMVar $ initialRoomState rId registryManage music
  return $ Room {
      enterRoom         = enterRoomImpl        stateVar
    , getUser           = getUserImpl          stateVar
    , leaveRoom         = leaveRoomImpl        stateVar
    , viewRoom          = viewRoomImpl         stateVar
    , getMusic          = getMusicImpl         stateVar
    , nextUp            =  nextUpImpl       stateVar
    }

-- Room API Impls

enterRoomImpl :: MVar RoomState -> Connection IO -> IO ()
enterRoomImpl stateVar pend = do
  conn <- WS.acceptRequest pend
  uId  <-
    modifyMVar stateVar $ \st -> do
      let uId  =      userCount st
      u        <-     newUser (roomId st) uId (uId == 0)
      rUser    <-     getRoomUser u
      let st'  =      addUserToRoom st (userId rUser) (USession conn u)
      messageToUser   st' (userId rUser) (ServerWelcomeMessage rUser)
      publishToAllBut st' (/= rUser)     (UserEnterEvent rUser)
      return  (st'{userCount=uId + 1}, uId)
  WS.withPingThread conn 30 (return ()) $
    handleIncomingMessages stateVar conn uId
  -- todo: deal with async threads
  -- we should keep a reference to the thread so when room is empty we can terminate it 
  --

getMusicImpl :: MVar RoomState -> IO (Music IO)
getMusicImpl stateVar = music <$> readMVar stateVar

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
     selfDestructCallback $ registryManage st

viewRoomImpl :: MVar RoomState -> IO [RoomUser]
viewRoomImpl stateVar = do
  roomState <- readMVar stateVar
  let ks = [0 .. userCount roomState]
  let userSessions = Data.List.map (roomUsers roomState Map.!) ks
  users <- mapM (getRoomUser . user) userSessions
  return $ sort users

nextUpImpl :: MVar RoomState -> IO (User IO)
nextUpImpl stateVar = do
  modifyMVar stateVar $ \st -> do
    let curr = nextPos st
    messageToUser st curr (ServerUploadSong "")
    let u = user $ roomUsers st Map.! curr
    return (st{nextPos=curr+1}, u)



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

-- Pure functions

addUserToRoom :: RoomState -> UserId -> UserSession -> RoomState
addUserToRoom st@(RoomState _ users _ _ _ _ _) uId uSession =
  st{roomUsers = Map.insert uId uSession users}

removeUser :: RoomState -> UserId -> RoomState
removeUser st@(RoomState _ users _ _ _ _ _) uId =
  st{roomUsers= Map.delete uId users}
