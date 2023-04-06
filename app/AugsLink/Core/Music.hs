module AugsLink.Core.Music 
  ( 
    newMusic
  ) where

import Control.Concurrent
import Control.Monad
import Data.Text

import qualified Data.HashMap.Lazy as Map
import qualified Network.WebSockets as WS

import AugsLink.Core.API

type instance Connection IO = WS.PendingConnection

data UserListenSession = ULSession
  {
    conn :: WS.Connection
  , user :: User IO
  }

data PlayState = NotStarted | Playing

data MusicState = MusicState
  {
    playState        :: PlayState
  , listening        :: Map.HashMap UserId UserListenSession
  , currentlyPlaying :: Maybe Song
  }

-- 1)Maybe I can use a media player on the host to get a sort of signal to how far along the song is...
-- 2) Maybe The media players in the browser can send a pulse every second of the song which updates the music player here
--    I could then use this pulse to know when the song is done?
newMusic :: IO (Music IO)
newMusic = do
  stateVar <- newMVar $ MusicState NotStarted Map.empty Nothing
  return $ Music {
      listen        = listenImpl        stateVar
    , start         = startImpl         stateVar
    , stopListening = stopListeningImpl stateVar
    }

listenImpl :: MVar MusicState -> User IO -> Connection IO -> IO ()
listenImpl stateVar user pend = do
  conn  <-     WS.acceptRequest pend
  uId <-     userId <$> getRoomUser user
  modifyMVar_ stateVar $ \st -> do
    let u   = ULSession conn user
    let st' = st{listening= Map.insert uId u (listening st)}
    return st'
  WS.withPingThread conn 30 (return ()) $ 
    handleIncomingMessages stateVar conn uId

startImpl :: MVar MusicState -> User IO -> IO ()
startImpl stateVar user = do
  createdRoom <- isCreator user
  if createdRoom then do
    modifyMVar_ stateVar $ \st ->
      return st{playState=Playing}
    stream
  else error "This user did not create the room"
  where
    stream :: IO ()
    stream  = do 
      modifyMVar_ stateVar $ \st ->
        case playState st of
          Playing -> do
            forM_ (listening st) $ \session ->
              case currentlyPlaying st of
                Just s -> do
                  WS.sendTextData (conn session) (songTitle $ songInfo s)
                  threadDelay 1000000 -- 1 second
                Nothing -> return ()
            return st

          _ -> return st
      stream

stopListeningImpl :: MVar MusicState -> User IO -> IO ()
stopListeningImpl stateVar user = do
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
