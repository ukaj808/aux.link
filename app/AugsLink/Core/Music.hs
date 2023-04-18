{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Core.Music
  (
    newMusic
  ) where

import Control.Concurrent
import Control.Monad
import Data.Time.Clock
import GHC.IO.Handle
import System.IO


import qualified Data.ByteString    as B
import qualified Data.HashMap.Lazy  as Map
import qualified Network.WebSockets as WS

import AugsLink.Core.API
import AugsLink.Core.FFMpeg
import Data.Maybe (fromMaybe)
import Data.Int

type instance Connection IO = WS.PendingConnection

newtype UserListenSession = ULSession
  {
    conn :: WS.Connection
  }

data SongState = SongState
  {
    chunkDelayMs     :: Int
  , chunkSizeBytes   :: Int
  , duration         :: Double
  , fileHandle       :: Handle
  , filePath         :: FilePath
  , sId              :: SongId
  , startTime        :: UTCTime
  , endTime          :: UTCTime
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
      -- Does user have song queued? Lets pull it
      case next of
        -- Yes they do, lets take it
        Right (Just sId) -> do
          -- Lets put there 'possible' next song in the tape player
          sngSt <- initializeSongState sId
          currentlyPlaying <- modifyMVar stateVar $ \st -> do
            return (st{currentlyPlaying=Just sngSt}, sngSt)
          stream currentlyPlaying
          nextSong
          where
            stream :: SongState -> IO ()
            stream sngSt = do
              currentTime <- getCurrentTime
              if currentTime >= endTime sngSt
              then return ()
              else do
                st <- readMVar stateVar
                forM_ (listening st) $ \session -> do
                  bs <- B.hGet (fileHandle sngSt) (chunkSizeBytes sngSt)
                  WS.sendBinaryData (conn session) bs
                threadDelay $ chunkDelayMs sngSt
                stream sngSt
        -- Either they dont have any or something failed on upload
        Right Nothing  -> nextSong
        Left err -> undefined


initializeSongState :: SongId -> IO SongState
initializeSongState sId = do
  ffpd <- ffprobe "ffprobe" "./static/song.mp3"
  let filePath = "./static/song.mp3"
  handle <- openBinaryFile filePath ReadMode
  currentTime <- getCurrentTime
  let dur = fromMaybe (error "Coudlnt derive duration of the song") (formatDuration $ format ffpd)
  return $ SongState {
      chunkDelayMs  = calcChunkDelay ffpd
    , chunkSizeBytes= calcChunkSize ffpd
    , duration      = dur
    , endTime       = addUTCTime (realToFrac dur :: NominalDiffTime) currentTime 
    , fileHandle    = handle
    , filePath      = filePath
    , sId           = sId
    , startTime     = currentTime
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

calcChunkDelay :: FFProbeData -> Int
calcChunkDelay = undefined

calcChunkSize :: FFProbeData -> Int
calcChunkSize = undefined

