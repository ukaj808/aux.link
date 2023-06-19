{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Core.Music
  (
    newMusicStreamer
  ) where

import Control.Concurrent
import Control.Monad
import GHC.IO.Handle
import Servant.Multipart
import System.Directory

import qualified Data.ByteString    as B
import qualified Data.HashMap.Lazy  as Map
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import AugsLink.Core.API
import Commons.Wav (FmtSubChunk (byteRate, FmtSubChunk), parseWavFile)
import System.FilePath
import Commons.FFMpeg
import System.IO

type instance Connection IO = WS.PendingConnection
type instance SongFile IO   = MultipartData Tmp

newtype UserListenSession = ULSession
  {
    conn :: WS.Connection
  }


newtype MusicStreamerState = MusicState
  {
    listening        :: Map.HashMap UserId UserListenSession
  }

newMusicStreamer :: RoomId -> IO (MusicStreamer IO)
newMusicStreamer rId = do
  stateVar <- newMVar $ MusicState Map.empty
  createDirectoryIfMissing True ("./rooms/" ++ T.unpack rId)
  return $ Music {
      listen             = listenImpl         stateVar
    , stream             = streamImpl         stateVar
    }

listenImpl :: MVar MusicStreamerState -> UserId -> Connection IO -> IO ()
listenImpl stateVar uId pend = do
  conn  <-     WS.acceptRequest pend
  modifyMVar_ stateVar $ \st -> do
    let u   = ULSession conn
    let st' = st{listening= Map.insert uId u (listening st)}
    return st'
  WS.withPingThread conn 30 (return ()) $
    handleIncomingMessages stateVar conn uId

streamImpl :: MVar MusicStreamerState -> FilePath -> RoomId -> IO ()
streamImpl stateVar file rId = do
  let fileName = takeBaseName file
  let fileExt  = takeExtension file
  wavFile   <- convertToWav "ffmpeg" ("./rooms/" ++ T.unpack rId) fileName fileExt
  handle    <- openFile wavFile ReadMode
  (fmtSubChunk, audioByteLength) <- parseWavFile handle
  st <- readMVar stateVar
  forM_ (listening st) $ \session -> do
    WS.sendBinaryData (conn session) (B.pack [1])
  go fmtSubChunk handle audioByteLength
  forM_ (listening st) $ \session -> do
    WS.sendBinaryData (conn session) (B.pack [0])
  where 
    go :: FmtSubChunk -> Handle -> Integer -> IO ()
    go _ _ bytesLeft | bytesLeft <= 0 = return () 
    go fmtSubChunk handle bytesLeft = do
      let chunkSizeInt :: Int = 16384
      let byteRateFloat :: Float = fromIntegral $ byteRate fmtSubChunk
      let delay:: Int =  round $ (fromIntegral chunkSizeInt / byteRateFloat) * 1000000
      st <- readMVar stateVar
      chunk <- B.hGet handle chunkSizeInt
      forM_ (listening st) $ \session -> do
        WS.sendBinaryData (conn session) chunk
      threadDelay delay
      go fmtSubChunk handle (bytesLeft - toInteger chunkSizeInt)

handleIncomingMessages :: MVar MusicStreamerState -> WS.Connection -> UserId -> IO ()
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
