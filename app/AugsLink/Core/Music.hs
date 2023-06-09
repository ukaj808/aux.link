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
import System.Directory
import System.FilePath

type instance Connection IO = WS.PendingConnection
type instance SongFile IO   = MultipartData Tmp

newtype UserListenSession = ULSession
  {
    conn :: WS.Connection
  }


data MusicState = MusicState
  {
    songInPlayer     :: Maybe T.Text
  , listening        :: Map.HashMap UserId UserListenSession
  , currentlyPlaying :: Maybe String
  }

newMusic :: RoomId -> IO (Music IO)
newMusic rId = do
  stateVar <- newMVar $ MusicState Nothing Map.empty Nothing
  createDirectoryIfMissing True ("./rooms/" ++ T.unpack rId)
  return $ Music {
      listen             = listenImpl        stateVar
    , start              = startImpl         stateVar
    , putSongInPlayer    = putSongInPlayerImpl stateVar
    }

putSongInPlayerImpl :: MVar MusicState -> RoomId -> SongFile IO -> IO ()
putSongInPlayerImpl stateVar rId sFile = do
  let parse = lookupFile "file" sFile
  either (error "Could not find song in file upload") 
    (\s -> do 
      storeSongInDisk rId s
      let fileName = fdFileName s
      modifyMVar_ stateVar $ \st -> do
        return st{songInPlayer=Just fileName}
      ) parse

storeSongInDisk :: RoomId -> FileData Tmp -> IO ()
storeSongInDisk rId sFile = do
  let fileName = fdFileName sFile
      sourcePath = fdPayload sFile
      targetPath = "./rooms/" ++ T.unpack rId ++ "/" ++ T.unpack fileName
  copyFile sourcePath targetPath
      
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
      polled <- pollSongInPlayer stateVar 5
      case polled of
        Nothing -> do
          putStrLn "No song in player"
          nextSong
        Just file -> do
          let fileName = takeBaseName $ T.unpack file
          let fileExt = takeExtension $ T.unpack file
          putStrLn $ "Playing song: " ++ fileName
          putStrLn $ "File extension: " ++ fileExt
          wavFile   <- convertToWav "ffmpeg" ("./rooms/" ++ T.unpack rId) fileName fileExt
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

pollSongInPlayer :: MVar MusicState -> Int -> IO (Maybe T.Text)
pollSongInPlayer _ 0 = return Nothing
pollSongInPlayer stateVar retryAttempts = do
  st <- readMVar stateVar
  case songInPlayer st of
    Nothing -> do
      threadDelay 1000000            
      pollSongInPlayer stateVar (retryAttempts - 1)
    Just fileName  -> return $ Just fileName


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
