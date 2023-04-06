module AugsLink.Core.Shared 
  ( 
    RegistryManage(..) 
  , MusicFns
  ) where
import AugsLink.Core.API

newtype RegistryManage = RegistryManage {
  selfDestructCallback :: IO ()
}

newtype MusicFns = MusicFns {
  songEnded :: IO ()
}
