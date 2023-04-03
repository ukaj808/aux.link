module AugsLink.Core.Music 
  ( 
    newMusic
  ) where

import Control.Concurrent

import AugsLink.Core.API

data SongState = SongState
  {
    duration :: Int
  , elapsed  :: Int
  , file     :: Int
  }

data MusicState = MusicState
  {
    currentSong :: Maybe SongState
  }

-- 1)Maybe I can use a media player on the host to get a sort of signal to how far along the song is...
-- 2) Maybe The media players in the browser can send a pulse every second of the song which updates the music player here
--    I could then use this pulse to know when the song is done?
newMusic :: IO (Music IO)
newMusic = do
  stateVar <- newMVar $ MusicState Nothing
  return $ Music {
      start            = startImpl stateVar
    , currentlyPlaying = undefined
    , confirmReady     = undefined
    }

startImpl :: MVar MusicState -> IO ()
startImpl stateVar = do
  sync
  stream


sync :: IO ()
sync = undefined

stream :: IO ()
stream = undefined
