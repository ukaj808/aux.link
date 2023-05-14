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
          (fmtSubChunk, audioSizeInBytes) <- parseWavFile handle
          print fmtSubChunk
          modifyMVar_ stateVar $ \st -> do
            return st{currentlyPlaying=Just sId} 
          liveStream fmtSubChunk handle
          nextSong
          where
            liveStream :: FmtSubChunk -> Handle -> IO ()
            liveStream fmtSubChunk handle = do
              playing <- not <$> hIsEOF handle
              when playing $ do
                st <- readMVar stateVar
                forM_ (listening st) $ \session -> do
                  let numBytes = div (fromIntegral $ byteRate fmtSubChunk) 8
                  chunk <- B.hGet handle numBytes -- 1/8th a second of audio
                  WS.sendBinaryData (conn session) chunk
                threadDelay 125000 -- 1/8th of a second
                liveStream fmtSubChunk handle


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
