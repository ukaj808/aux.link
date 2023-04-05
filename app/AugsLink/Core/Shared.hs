module AugsLink.Core.Shared 
  ( 
    RegistryManage(..) 
  , RoomManage(..)
  ) where
import AugsLink.Core.API

newtype RegistryManage = RegistryManage {
  selfDestructCallback :: IO ()
}

data RoomManage = RoomManage {
  startMusicCallback        :: Either String (IO ())
, listenToMusicCallback     :: Connection IO -> IO ()
, stopListenToMusicCallback :: IO ()
}
