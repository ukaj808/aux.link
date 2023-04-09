module AugsLink.Core.Music
  (
    newMusic
  ) where

import Control.Concurrent
import Control.Monad

import qualified Data.HashMap.Lazy as Map
import qualified Network.WebSockets as WS

import AugsLink.Core.API
import Data.Text

type instance Connection IO = WS.PendingConnection

data UserListenSession = ULSession
  {
    conn :: WS.Connection
  , user :: User IO
  }

data PlayState = NotStarted | Playing

data SongState = SongState
  {
    song        :: Song
  , timeElapsed :: Int
  }

data MusicState = MusicState
  {
    started          :: Bool
  , listening        :: Map.HashMap UserId UserListenSession
  , currentlyPlaying :: Maybe SongState
  }

-- 1)Maybe I can use a media player on the host to get a sort of signal to how far along the song is...
-- 2) Maybe The media players in the browser can send a pulse every second of the song which updates the music player here
--    I could then use this pulse to know when the song is done?
newMusic :: IO (Music IO)
newMusic = do
  stateVar <- newMVar $ MusicState False Map.empty Nothing
  return $ Music {
      listen        = listenImpl        stateVar
    , start         = startImpl         stateVar
    , stopListening = stopListeningImpl stateVar
    }

uploadSongImpl :: MVar MusicState -> SongId -> SongFile IO -> IO ()
uploadSongImpl sId sFile = undefined

listenImpl :: MVar MusicState -> Room IO -> User IO -> Connection IO -> IO ()
listenImpl stateVar room user pend = do
  conn  <-     WS.acceptRequest pend
  uId <-     userId <$> getRoomUser user
  modifyMVar_ stateVar $ \st -> do
    let u   = ULSession conn user
    let st' = st{listening= Map.insert uId u (listening st)}
    return st'
  WS.withPingThread conn 30 (return ()) $
    handleIncomingMessages stateVar conn uId

startImpl :: MVar MusicState -> Room IO -> User IO -> IO ()
startImpl stateVar room user = do
  createdRoom <- isCreator user
  if createdRoom then do
    modifyMVar_ stateVar $ \st ->
      return st{started=True}
    nextSong
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
          cp <- modifyMVar stateVar $ \st -> do
            let streamFile = undefined
            let sngSt = undefined
            return (st{currentlyPlaying=Just sngSt}, sngSt)
          stream cp
          nextSong
          where
            stream :: SongState -> IO ()
            stream sngSt = do
              if timeElapsed sngSt >= songLength (songInfo $ song sngSt)
              then return ()
              else do
                modifyMVar_ stateVar $ \st -> do
                  forM_ (listening st) $ \session ->
                    WS.sendTextData (conn session) (pack "streaming!")
                  return st{currentlyPlaying=Just sngSt{timeElapsed=timeElapsed sngSt + 1}}
                stream sngSt
        -- Either they dont have any or something failed on upload
        Right Nothing  -> nextSong
        Left err -> undefined


stopListeningImpl :: MVar MusicState -> Room IO -> User IO -> IO ()
stopListeningImpl stateVar room user = do
  uId <-     userId <$> getRoomUser user
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
        WS.ControlMessage _ -> go
