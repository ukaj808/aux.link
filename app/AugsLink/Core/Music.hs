module AugsLink.Core.Music 
  ( 
    newMusic
  ) where

import AugsLink.Core.API

-- 1)Maybe I can use a media player on the host to get a sort of signal to how far along the song is...
-- 2) Maybe The media players in the browser can send a pulse every second of the song which updates the music player here
--    I could then use this pulse to know when the song is done?
newMusic :: IO (Music IO)
newMusic =
  return $ Music {
      start            = undefined
    , currentlyPlaying = undefined
    , confirmReady     = undefined
    , tick = undefined  
    }
