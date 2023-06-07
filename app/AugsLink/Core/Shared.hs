module AugsLink.Core.Shared 
  ( 
    RegistryManage(..)  ,
    RoomCallback(..)
  ) where
import AugsLink.Core.API


newtype RegistryManage = RegistryManage {
  selfDestructCallback :: IO ()
}

newtype RoomCallback = RoomCallback {
  publishToRoomCb :: RoomEvent -> IO ()
}
