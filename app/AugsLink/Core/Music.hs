{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Core.Music
  (
    newMusic
  ) where

import Control.Concurrent
import Control.Monad
import GHC.IO.Handle

import qualified Data.ByteString    as B
import qualified Data.HashMap.Lazy  as Map
import qualified Network.WebSockets as WS

import AugsLink.Core.API
import AugsLink.Core.FFMpeg
import AugsLink.Core.Wav
import System.IO
import Servant.Multipart

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import System.Directory

type instance Connection IO = WS.PendingConnection
type instance SongFile IO   = MultipartData AudioFile

newtype UserListenSession = ULSession
  {
    conn :: WS.Connection
  }

data MusicState = MusicState
  {
    songInPlayer     :: Bool
  , listening        :: Map.HashMap UserId UserListenSession
  , currentlyPlaying :: Maybe String
  }

newMusic :: RoomId -> IO (Music IO)
newMusic rId = do
  stateVar <- newMVar $ MusicState False Map.empty Nothing
  createDirectoryIfMissing True ("./rooms/" ++ T.unpack rId)
  return $ Music {
      listen             = listenImpl        stateVar
    , start              = startImpl         stateVar
    , putSongInPlayer    = putSongInPlayerImpl stateVar
    }

putSongInPlayerImpl :: MVar MusicState -> RoomId -> SongFile IO -> IO ()
putSongInPlayerImpl stateVar rId sFile = do
  storeSongInDisk rId sFile
  modifyMVar_ stateVar $ \st -> do
    return st{songInPlayer=True}

storeSongInDisk :: RoomId -> SongFile IO -> IO ()
storeSongInDisk rId sFile = do
  let sourcePath = tmpPath (files sFile)
      targetPath = "./" ++ T.unpack rId ++ "/" ++ T.unpack "songname"
  renameFile sourcePath targetPath

listenImpl :: MVar MusicState -> UserId -> Connection IO -> IO ()
listenImpl stateVar uId pend = do
  conn  <-     WS.acceptRequest pend
  modifyMVar_ stateVar $ \st -> do
    let u   = ULSession conn
    let st' = st{listening= Map.insert uId u (listening st)}
    return st'
  WS.withPingThread conn 30 (return ()) $
    handleIncomingMessages stateVar conn uId

startImpl :: MVar MusicState -> RoomId -> IO ()
startImpl stateVar rId = do
  _ <- forkIO nextSong
  return ()
  where

    nextSong  :: IO ()
    nextSong  = do
      countdownToSongStart stateVar
      _ <- pollSongInPlayer stateVar 5
      wavFile   <- convertToWav "ffmpeg" ("./rooms/" ++ T.unpack rId) "songname" "mp3"
      handle    <- openFile wavFile ReadMode
      (fmtSubChunk, audioByteLength) <- parseWavFile handle
      print fmtSubChunk
      let byteRateMs :: Int = div (fromIntegral (byteRate fmtSubChunk)) 1000
      let chunkSize = byteRateMs * 200
      modifyMVar_ stateVar $ \st -> do
        return st{currentlyPlaying=Just "songName etc..."}
      liveStream (audioByteLength, chunkSize) handle
      nextSong
      where

        liveStream :: (Integer, Int) -> Handle -> IO ()
        liveStream (bytesLeft, chunkSize) handle = do
          when (bytesLeft > 0) $ do
            st <- readMVar stateVar
            forM_ (listening st) $ \session -> do
              chunk <- B.hGet handle chunkSize
              WS.sendBinaryData (conn session) chunk
            threadDelay 200000
            liveStream (bytesLeft - toInteger chunkSize, chunkSize) handle

pollSongInPlayer :: MVar MusicState -> Int -> IO Bool
pollSongInPlayer _ 0 = return False
pollSongInPlayer stateVar retryAttempts = do
  st <- readMVar stateVar
  if songInPlayer st
  then do
    return True
  else do
    threadDelay 1000000
    pollSongInPlayer stateVar (retryAttempts - 1)


countdownToSongStart :: MVar MusicState -> IO ()
countdownToSongStart stateVar = do
  forM_ [5,4,3,2,1,0] $ \i -> do
    st <- readMVar stateVar
    forM_ (listening st) $ \session -> do
      WS.sendBinaryData (conn session) (B.pack [i])
    threadDelay 1000000

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
