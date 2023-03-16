module AugsLink.Core.Registry ( SelfManage(..), newRegistry ) where
import qualified Data.HashMap.Strict as HM

import Control.Concurrent
import AugsLink.Core.API (RoomId, Room, Registry (..))
import Data.UUID.V4
import Data.UUID
import AugsLink.Core.Room (newRoom)
import AugsLink.Core.Shared
import System.Directory (createDirectoryIfMissing, removePathForcibly)

newtype RegistryState = RegistryState
  {
    rooms :: HM.HashMap RoomId (Room IO)
  }


initialRegistryState :: RegistryState
initialRegistryState = RegistryState 
  {
    rooms=HM.empty
  }

newRegistry :: IO (Registry IO)
newRegistry = do
  stateVar        <- newMVar initialRegistryState
  return $ Registry
    {
      numRooms =
        HM.size       . rooms <$> readMVar stateVar
    , getRoom = \rId ->
        HM.lookup rId . rooms <$> readMVar stateVar
    , createRoom = do
        rId  <- nextRandom
        room <- newRoom rId (SelfManage {selfDestruct=deleteRoomImpl stateVar rId})
        roomCount <- modifyMVar stateVar $ \st -> do
          let rooms' = HM.insert rId room $ rooms st
          return (st{rooms =  rooms'}, HM.size rooms')
        createDirectoryIfMissing True ("./rooms/" ++ toString rId)
        print $ show roomCount ++ " rooms now after creating room " ++ toString rId
        return rId
    , deleteRoom = deleteRoomImpl stateVar
    }

deleteRoomImpl :: MVar RegistryState -> RoomId -> IO ()
deleteRoomImpl stateVar rId = do
  roomCount <- modifyMVar stateVar $ \st -> do
    let rooms' = HM.delete rId $ rooms st
    return (st{rooms = rooms'}, HM.size rooms')
  removePathForcibly ("./rooms/" ++ toString rId)
  print $ show roomCount ++ " rooms left after deleting room " ++ toString rId
