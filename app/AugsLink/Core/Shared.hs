module AugsLink.Core.Shared 
  ( 
    RegistryManage(..) 
  , RoomManage(..)
  ) where

newtype RegistryManage = RegistryManage {
  selfDestructCallback :: IO ()
}

newtype RoomManage = RoomManage {
  startMusicCallback :: IO ()
}
