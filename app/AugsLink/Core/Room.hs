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
import Servant.Multipart

import qualified Data.Aeson           as Aeson
import qualified Data.Map    as Map
import qualified Network.WebSockets   as WS

import AugsLink.Core.API
import AugsLink.Core.Music
import AugsLink.Core.Shared
import AugsLink.Core.User
import qualified Data.Text as T
import System.Directory
import Control.Concurrent
import System.FilePath
import AugsLink.Core.FFMpeg
import System.IO
import AugsLink.Core.Wav
import System.Random (RandomGen(next))

type instance Connection IO             = WS.PendingConnection
type instance SongFile IO     = MultipartData Tmp

data RoomState = RoomState
  {
    roomId                       :: RoomId
  , roomUsers                    :: Map.Map UserId UserSession
  , registryManage               :: RegistryManage
  , musicStreamer                :: MusicStreamer IO
  , creator                      :: Maybe UserId
  , nextPos                      :: Int
  , userCount                    :: Int
  , turn                         :: Int
  , currentSong                  :: Maybe T.Text
  }

data UserSession = USession
  {
    conn :: WS.Connection
  , user :: User IO
  }

initialRoomState :: RoomId -> RegistryManage -> MusicStreamer IO -> RoomState
initialRoomState rId rsm music = RoomState
  {
    roomId         = rId
  , roomUsers      = Map.empty
  , registryManage = rsm
  , musicStreamer          = music
  , creator        = Nothing
  , nextPos     = 0
  , userCount      = 0
  , turn          = 0
  , currentSong   = Nothing
  }

newRoom :: RoomId -> RegistryManage -> IO (Room IO)
newRoom rId registryManage = do
  music    <- newMusic rId
  stateVar <- newMVar $ initialRoomState rId registryManage music
  return $ Room {
      enterRoom         = enterRoomImpl        stateVar
    , getUser           = getUserImpl          stateVar
    , leaveRoom         = leaveRoomImpl        stateVar
    , viewRoom          = viewRoomImpl         stateVar
    , getMusic          = getMusicImpl         stateVar
    , getCreatorId      = creator <$> readMVar stateVar
    , startMusic        = startMusicImpl       stateVar
    , uploadSong        = uploadSongImpl       stateVar
    }

-- Room API Impls

startMusicImpl :: MVar RoomState -> UserId -> IO ()
startMusicImpl stateVar uId = do
  st <- readMVar stateVar
  case creator st of
    Just cId | cId == uId -> do
      _ <- forkIO nextSong
      return ()
      where
      nextSong  :: IO ()
      nextSong  = do
        countdownToSongStart stateVar -- Send message to all users; counting down from five; on the Room chhanel
        nextUp <- nextUser stateVar -- Get the next user to play music
        messageToUser st nextUp ServerUploadSong -- Send message to the user; telling them to start the music
        polled <- pollSongIsUploaded stateVar 5 -- Poll the music player to see if the song has been uploaded by the user
        case polled of
          Nothing -> do
            putStrLn "No song in player"
            nextSong
          Just file -> do
            let fileName = takeBaseName $ T.unpack file
            let fileExt = takeExtension $ T.unpack file
            wavFile   <- convertToWav "ffmpeg" ("./rooms/" ++ T.unpack (roomId st)) fileName fileExt

            handle    <- openFile wavFile ReadMode
            (fmtSubChunk, audioByteLength) <- parseWavFile handle
            print fmtSubChunk
            let byteRateMs :: Int = div (fromIntegral (byteRate fmtSubChunk)) 1000
            let chunkSize = byteRateMs * 200
            -- send another message to all users; telling them the song specifidcs so they can init there audioplayers

            stream (musicStreamer st) (audioByteLength, chunkSize) handle
            nextSong
    _                     -> error "Only the creator can start the music" 

nextUser :: MVar RoomState -> IO UserId
nextUser stateVar = undefined

pollSongIsUploaded :: MVar RoomState -> Int -> IO (Maybe T.Text)
pollSongIsUploaded _ 0 = return Nothing
pollSongIsUploaded stateVar retryAttempts = do
  st <- readMVar stateVar
  case currentSong st of
    Nothing -> do
      threadDelay 1000000            
      pollSongIsUploaded stateVar (retryAttempts - 1)
    Just fileName  -> return $ Just fileName

countdownToSongStart :: MVar RoomState -> IO ()
countdownToSongStart stateVar = do
  forM_ [5,4,3,2,1,0] $ \i -> do
    st <- readMVar stateVar
    publishToRoom st (SongStartingEvent i)
    threadDelay 1000000
--Only allows uploads when its your turn

uploadSongImpl :: MVar RoomState -> UserId -> SongFile IO -> IO ()
uploadSongImpl stateVar uId song = do
  st <- readMVar stateVar
  if uId == turn st then do
    let parse = lookupFile "file" song
    either (error "Could not find song in file upload") 
      (\s -> do 
        
        let fileName = fdFileName s
            sourcePath = fdPayload s
            targetPath = "./rooms/" ++ T.unpack (roomId st) ++ "/" ++ T.unpack fileName
        copyFile sourcePath targetPath
        ) parse 
  else
    error "Not your turn"

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
      messageToUser   st' (userId rUser) (ServerWelcomeMessage rUser)
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
addUserToRoom st@(RoomState _ users _ _ _ _ _ _ _) uId uSession =
  st{roomUsers = Map.insert uId uSession users}

removeUser :: RoomState -> UserId -> RoomState
removeUser st@(RoomState _ users _ _ _ _ _ _ _) uId =
  st{roomUsers= Map.delete uId users}
