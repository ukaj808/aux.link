{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Core.Music
  (
    newMusic
  ) where

import Control.Concurrent
import Control.Monad
import Foreign
import GHC.IO.Handle
import System.IO

import qualified Data.ByteString    as B
import qualified Data.HashMap.Lazy  as Map
import qualified Network.WebSockets as WS

import AugsLink.Core.API
import AugsLink.Core.FFMpeg

type instance Connection IO = WS.PendingConnection

newtype UserListenSession = ULSession
  {
    conn :: WS.Connection
  }

data SongState = SongState
  {
    song        :: Song
  , timeElapsed :: Int
  , filePath    :: FilePath
  , fileHandle  :: Handle
  , fileBuffer  :: Ptr Int8
  , ffprobeData :: FFProbeData
  }

data MusicState = MusicState
  {
    started          :: Bool
  , listening        :: Map.HashMap UserId UserListenSession
  , currentlyPlaying :: Maybe SongState
  }

newMusic :: IO (Music IO)
newMusic = do
  stateVar <- newMVar $ MusicState False Map.empty Nothing
  return $ Music {
      listen        = listenImpl        stateVar
    , start         = startImpl         stateVar
    , stopListening = stopListeningImpl stateVar
    }

listenImpl :: MVar MusicState -> UserId -> Connection IO -> IO ()
listenImpl stateVar uId pend = do
  conn  <-     WS.acceptRequest pend
  modifyMVar_ stateVar $ \st -> do
    let u   = ULSession conn
    let st' = st{listening= Map.insert uId u (listening st)}
    return st'
  WS.withPingThread conn 30 (return ()) $
    handleIncomingMessages stateVar conn uId

startImpl :: MVar MusicState -> Room IO -> UserId -> IO ()
startImpl stateVar room uId = do
  creatorId <- getCreatorId room
  let createdRoom = case creatorId of
                     Just userId -> userId == uId
                     Nothing     -> False
  if createdRoom then do
    modifyMVar_ stateVar $ \st ->
      return st{started=True}
    _ <- forkIO nextSong
    return ()
  else error "This user did not create the room"
  where
    nextSong  :: IO ()
    nextSong  = do
      nxtUser <-  nextUp room
      next    <-  dequeueSong nxtUser
      print next
      -- Does user have song queued? Lets pull it
      case next of
        -- Yes they do, lets take it
        Right (Just sId) -> do
          -- Lets put there 'possible' next song in the tape player
          sngSt <- initializeSongState sId (SongInfo "" "" 10000000)
          currentlyPlaying <- modifyMVar stateVar $ \st -> do
            return (st{currentlyPlaying=Just sngSt}, sngSt)
          stream currentlyPlaying
          nextSong
          where
            stream :: SongState -> IO ()
            stream sngSt = do
              if timeElapsed sngSt >= songLength (songInfo $ song sngSt)
              then return ()
              else do
                sngSt' <- modifyMVar stateVar $ \st -> do
                  forM_ (listening st) $ \session -> do
                    let h = fileHandle sngSt
                    let b = fileBuffer sngSt
                    i <- hGetBuf h b 8
                    bs <- peek b
                    let wsData = B.pack [ fromIntegral bs]
                    WS.sendBinaryData (conn session) wsData
                  let sngSt' = sngSt{timeElapsed = timeElapsed sngSt + 1}
                  return (st{currentlyPlaying=Just sngSt'}, sngSt')
                stream sngSt'
        -- Either they dont have any or something failed on upload
        Right Nothing  -> nextSong
        Left err -> undefined


initializeSongState :: SongId -> SongInfo -> IO SongState
initializeSongState sId sInfo = do
  ffmpegConvertAudio "ffmpeg" "./static/song.mp3" "./static/song.wav"
  ffpd <- ffprobeAudio "ffprobe" "./static/song.wav"
  let filePath = "./static/song.wav"
  handle <- openBinaryFile filePath ReadMode
  let song = Song {
    songInfo = sInfo 
  , songId = sId
  }
  buffer <- mallocBytes 8
  return $ SongState {
     timeElapsed=0
    ,filePath=filePath
    ,fileHandle=handle
    ,fileBuffer=buffer
    ,song=song
    ,ffprobeData=ffpd
  }

stopListeningImpl :: MVar MusicState -> UserId -> IO ()
stopListeningImpl stateVar uId = do
  modifyMVar_ stateVar $ \st ->
    return st{listening= Map.delete uId (listening st)}

handleIncomingMessages :: MVar MusicState -> WS.Connection -> UserId -> IO ()
handleIncomingMessages stateVar conn uId = go
  where
    go :: IO ()
    go  = do
      msg <- WS.receive conn
      case msg of
        WS.DataMessage {} -> do
          putStrLn "Should not be possible"
          go
        WS.ControlMessage WS.Close {} -> return ()
        WS.ControlMessage _ -> go
