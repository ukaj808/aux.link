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
import AugsLink.Core.FFMpeg (convertToWav)
import System.IO
import AugsLink.Core.Wav

type instance Connection IO = WS.PendingConnection

newtype UserListenSession = ULSession
  {
    conn :: WS.Connection
  }

data MusicState = MusicState
  {
    started          :: Bool
  , listening        :: Map.HashMap UserId UserListenSession
  , currentlyPlaying :: Maybe SongId
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
      -- Does user have song queued? Lets pull it
      case next of
        -- Yes they do, lets take it
        Right (Just sId) -> do
          -- Lets put there 'possible' next song in the tape player

          wavFile   <- convertToWav "ffmpeg" "./static" "song" "mp3"
          handle    <- openFile wavFile ReadMode
          (fmtSubChunk, audioByteLength) <- parseWavFile handle
          print fmtSubChunk
          let byteRateMs :: Int = div (fromIntegral (byteRate fmtSubChunk)) 1000
          let chunkSize = byteRateMs * 200
          modifyMVar_ stateVar $ \st -> do
            return st{currentlyPlaying=Just sId}
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


        -- Either they dont have any or something failed on upload
        Right Nothing  -> nextSong
        Left err -> undefined


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
