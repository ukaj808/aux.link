{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Core.Music
  (
    newMusicStreamer
  ) where

import Control.Concurrent
import Control.Monad
import GHC.IO.Handle
import System.Directory

import qualified Data.ByteString    as B
import qualified Data.HashMap.Lazy  as Map
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import AugsLink.Core.API
import System.FilePath
import Commons.FFMpeg
import System.IO
import Commons.Wav
import Data.UUID
import Data.UUID.V4

type instance Connection IO = WS.PendingConnection

data UserStreamState = Consuming | Waiting
  deriving (Eq, Show)

data UserStreamSession = UserStreamSession
  {
    conn :: WS.Connection
  , streamState :: UserStreamState
  }


newtype MusicStreamerState = MusicState
  {
    streams        :: Map.HashMap UserId UserStreamSession
  }

newMusicStreamer :: RoomId -> IO (MusicStreamer IO)
newMusicStreamer rId = do
  stateVar <- newMVar $ MusicState Map.empty
  createDirectoryIfMissing True ("./rooms/" ++ T.unpack rId)
  return $ Music {
      connect             = listenImpl         stateVar
    , stream             = streamImpl         stateVar
    }

listenImpl :: MVar MusicStreamerState -> Connection IO -> IO ()
listenImpl stateVar pend = do
  uId   <- toText <$> nextRandom
  conn  <-     WS.acceptRequest pend
  modifyMVar_ stateVar $ \st -> do
    let u   = UserStreamSession conn Waiting
    let st' = st{streams= Map.insert uId u (streams st)}
    return st'
  WS.withPingThread conn 30 (return ()) $
    handleIncomingMessages stateVar conn uId

streamImpl :: MVar MusicStreamerState -> FilePath -> RoomId -> IO ()
streamImpl stateVar file rId = do
  let fileName = takeBaseName file
  let fileExt  = takeExtension file
  wavFile   <- convertToWav "ffmpeg" ("./rooms/" ++ T.unpack rId) fileName fileExt
  handle    <- openFile wavFile ReadMode
  (fmtSubChunk,  rawFmtSubChunk, audioByteLength) <- parseWavFile handle
  modifyMVar_ stateVar $ \st -> do
    forM_ (streams st) $ \session -> do
      WS.sendBinaryData (conn session) rawFmtSubChunk
    return st{streams= Map.map (\session -> session{streamState=Consuming}) (streams st)}

  go fmtSubChunk handle audioByteLength

  modifyMVar_ stateVar $ \st -> do
    forM_ (streams st) $ \session -> do
      WS.sendBinaryData (conn session) (B.pack [0])
    return st
  waitUntilAllHaveConsumed stateVar
  where
    go :: FmtSubChunk -> Handle -> Integer -> IO ()
    go _ _ bytesLeft | bytesLeft <= 0 = return ()
    go fmtSubChunk handle bytesLeft = do
      let chunkSize = fromIntegral $ byteRate fmtSubChunk `div` 8
      let delay =  1000000 `div` 8
      st <- readMVar stateVar
      chunk <- B.hGet handle chunkSize
      forM_ (streams st) $ \session -> do
        WS.sendBinaryData (conn session) chunk
      threadDelay delay
      go fmtSubChunk handle (bytesLeft - toInteger chunkSize)

waitUntilAllHaveConsumed :: MVar MusicStreamerState -> IO ()
waitUntilAllHaveConsumed st = do
  st' <- readMVar st
  if areAllWaiting st'
  then return ()
  else do waitUntilAllHaveConsumed st

areAllWaiting :: MusicStreamerState -> Bool
areAllWaiting st = True
--areAllWaiting st = Map.foldl (\acc session -> acc && (streamState session == Waiting)) True (streams st)

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
        WS.ControlMessage WS.Close {} -> do
          modifyMVar_ stateVar $ \st -> do
            let st' = st{streams= Map.delete uId (streams st)}
            return st'
          go
        WS.ControlMessage _ -> go
