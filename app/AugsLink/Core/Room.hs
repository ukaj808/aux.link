{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Core.Room
  (
    initialRoomState
  , Room          (..)
  , RoomState     (..)
  , newRoom
  )
  where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.List
import Servant.Multipart

import System.Directory

import qualified Data.Aeson           as Aeson
import qualified Data.Text            as T
import qualified Data.Map             as Map
import qualified Network.WebSockets   as WS

import AugsLink.Core.API
import AugsLink.Core.Music
import AugsLink.Core.Shared
import AugsLink.Core.User
import Commons.YtDlp (ytdlpDownload, YtdlpOutput (ytdlpTitle))

type instance Connection IO = WS.PendingConnection

data RoomState = RoomState
  {
    roomId                       :: RoomId
  , roomUsers                    :: Map.Map UserId UserSession
  , registryManage               :: RegistryManage
  , musicStreamer                :: MusicStreamer IO
  , creator                      :: Maybe UserId
  , userCount                    :: Int
  , currentSong                  :: Maybe T.Text
  , order                        :: [UserId]
  , turn                         :: Int
  }

data UserSession = USession
  {
    conn :: WS.Connection
  , user :: User IO
  }

initialRoomState :: RoomId -> RegistryManage -> MusicStreamer IO -> RoomState
initialRoomState rId rsm musicStreamer = RoomState
  {
    roomId         = rId
  , roomUsers      = Map.empty
  , registryManage = rsm
  , musicStreamer  = musicStreamer
  , creator        = Nothing
  , userCount      = 0
  , currentSong    = Nothing
  , order          = []
  , turn           = -1
  }

newRoom :: RoomId -> RegistryManage -> IO (Room IO)
newRoom rId registryManage = do
  music    <- newMusicStreamer rId
  stateVar <- newMVar $ initialRoomState rId registryManage music
  return $ Room {
      enterRoom         = enterRoomImpl        stateVar
    , getUser           = getUserImpl          stateVar
    , leaveRoom         = leaveRoomImpl        stateVar
    , viewRoom          = viewRoomImpl         stateVar
    , getMusic          = getMusicImpl         stateVar
    , startMusic        = startMusicImpl       stateVar
    , uploadSong        = uploadSongImpl       stateVar rId
    }

startMusicImpl :: MVar RoomState -> UserId -> IO ()
startMusicImpl stateVar uId = do
  st <- readMVar stateVar
  case creator st of
    Just cId | cId == uId -> do
      _ <- forkIO $ nextSong stateVar
      return ()
    _ -> error "Only the creator can start the music"

nextSong  :: MVar RoomState -> IO ()
nextSong stateVar = do
  forM_ [5,4,3,2,1,0] $ \i -> do
    st' <- readMVar stateVar
    publishToRoom st' (SongStartingEvent i)
    threadDelay 1000000
  st     <- readMVar stateVar
  nextUp <- modifyMVar stateVar $ \st' -> return $ getNextUser st'
 -- Get the next user to play music
  messageToUser st nextUp ServerUploadSongCommand
  polled <- pollSongIsUploaded stateVar 5
  case polled of
    Nothing -> do
      putStrLn $ "No song uploaded withing timeframe by user: " ++ show nextUp
      nextSong stateVar
    Just file -> do
      publishToRoom st SongUploadedEvent
      stream (musicStreamer st) (T.unpack file) (roomId st)
      modifyMVar_ stateVar $ \st'' -> do
        return st''{currentSong=Nothing}
      nextSong stateVar


uploadSongImpl :: MVar RoomState -> RoomId -> UserId -> Upload IO -> IO ()
uploadSongImpl stateVar rId uId u = do
  st <- readMVar stateVar
  if uId == getTurnUser st then do
    n <- case u of 
        (DirectFileUpload (name, path)) -> do
          copyFile path (genTargetPath rId name)
          return $ T.pack name
        (UrlScrapeUpload url)-> do
          (vidFilePath, vidMetadata) <- ytdlpDownload "yt-dlp" (genTargetDir rId) url

          --todo: convert to audio
          -- store in room path
          return (ytdlpTitle vidMetadata)
    modifyMVar_ stateVar $ \st' -> do
      return st'{currentSong=Just n}
  else
    error "Not your turn"


genTargetDir :: RoomId -> FilePath
genTargetDir rId = "./rooms/" ++ T.unpack rId

genTargetPath :: RoomId -> String -> FilePath
genTargetPath rId fileName =  genTargetDir rId ++ "/" ++ fileName

enterRoomImpl :: MVar RoomState -> Connection IO -> IO ()
enterRoomImpl stateVar pend = do
  conn <- WS.acceptRequest pend
  uId  <-
    modifyMVar stateVar $ \st -> do
      let uId  =      userCount st
      u        <-     newUser (roomId st) uId (uId == 0)
      rUser    <-     getRoomUser u
      let st'  =      addUserToRoom st (userId rUser) (USession conn u)
      let c = case creator st of
               Just existing -> Just existing

               Nothing       -> Just uId
      messageToUser   st' (userId rUser) (ServerWelcomeCommand rUser)
      publishToAllBut st' (/= rUser)     (UserEnterEvent rUser)
      return  (st'{userCount=uId + 1, creator=c}, uId)
  WS.withPingThread conn 30 (return ()) $
    handleIncomingMessages stateVar conn uId
  -- todo: deal with async threads
  -- we should keep a reference to the thread so when room is empty we can terminate it 

getMusicImpl :: MVar RoomState -> IO (MusicStreamer IO)
getMusicImpl stateVar = musicStreamer <$> readMVar stateVar

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
  let userSessions = Map.elems $ roomUsers roomState
  users <- mapM (getRoomUser . user) userSessions
  return $ sort users

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
          go
        WS.ControlMessage _ -> go

publishToAllBut :: RoomState -> (RoomUser -> Bool) -> RoomEvent -> IO ()
publishToAllBut rmSt p e = do
  forM_ (roomUsers rmSt) $ \uSession -> do
    rUser <- getRoomUser $ user uSession
    when (p rUser) $ WS.sendTextData (conn uSession) (Aeson.encode e)

publishToRoom ::  RoomState -> RoomEvent -> IO ()
publishToRoom rmSt e = do
  Map.foldrWithKey
    (\uId uSession _ -> do
      safeSendTextData (conn uSession) uId (RoomEventMessage e))
      (return ())
      (roomUsers rmSt)


messageToUser :: RoomState -> UserId  -> ServerCommand -> IO ()
messageToUser rmSt uid cmd = do
  case roomUsers rmSt Map.!? uid of
    Nothing -> putStrLn $ "User not found: " ++ show uid
    Just uSession -> safeSendTextData (conn uSession) uid (ServerCommandMessage cmd)

safeSendTextData :: WS.Connection -> UserId -> Message -> IO ()
safeSendTextData conn uId d =
      catch (WS.sendTextData conn (Aeson.encode d))
            (\e -> do
              case e of
                WS.ConnectionClosed ->  putStrLn $ "Connection closed for user: "    ++ show uId
                _                   ->  putStrLn $ "Error sending message to user: " ++ show uId
              )

pollSongIsUploaded :: MVar RoomState -> Int -> IO (Maybe T.Text)
pollSongIsUploaded _ 0 = return Nothing
pollSongIsUploaded stateVar retryAttempts = do
  st <- readMVar stateVar
  case currentSong st of
    Nothing -> do
      threadDelay 1000000
      pollSongIsUploaded stateVar (retryAttempts - 1)
    Just fileName  -> return $ Just fileName

-- Pure functions
getNextUser :: RoomState -> (RoomState, UserId)
getNextUser st = (st{turn=turn'}, order st !! turn')
    where
      turn' = (turn st + 1) `mod` length (order st)

addUserToRoom :: RoomState -> UserId -> UserSession -> RoomState
addUserToRoom st@(RoomState _ users _ _ _ _ _ order _) uId uSession =
  st{roomUsers = Map.insert uId uSession users, order=order++[uId]}

removeUser :: RoomState -> UserId -> RoomState
removeUser st@(RoomState _ users _ _ _ _ _ _ _) uId =
  st{roomUsers= Map.delete uId users, order=filter (/= uId) $ order st}

getTurnUser :: RoomState -> UserId
getTurnUser st = order st !! turn st