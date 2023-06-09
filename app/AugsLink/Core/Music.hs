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
import Servant.Multipart

import qualified Data.Text as T
import System.Directory

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

newMusic :: RoomId -> IO (MusicStreamer IO)
newMusic rId = do
  stateVar <- newMVar $ MusicState Map.empty
  createDirectoryIfMissing True ("./rooms/" ++ T.unpack rId)
  return $ Music {
      listen             = listenImpl        stateVar
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

streamImpl :: MVar MusicStreamerState -> (Integer, Int) -> Handle -> IO ()
streamImpl stateVar (bytesLeft, chunkSize) h = do
    when (bytesLeft > 0) $ do
      st <- readMVar stateVar
      forM_ (listening st) $ \session -> do
        chunk <- B.hGet h chunkSize
        WS.sendBinaryData (conn session) chunk
      threadDelay 200000
      streamImpl stateVar (bytesLeft - toInteger chunkSize, chunkSize) h

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
