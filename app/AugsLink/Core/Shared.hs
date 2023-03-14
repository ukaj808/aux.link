module AugsLink.Core.Shared ( SelfManage(..) ) where

newtype SelfManage = SelfManage {
  selfDestruct :: IO ()
}
