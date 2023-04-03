module AugsLink.Core.Registry 
  ( 
    RegistryManage(..)
  , newRegistry
  ) where


import Control.Concurrent
import Data.UUID
import Data.UUID.V4
import Data.Text
import System.Directory

import qualified Data.HashMap.Lazy as Map

import AugsLink.Core.API
import AugsLink.Core.Room
import AugsLink.Core.Shared

newtype RegistryState = RegistryState
  {
    rooms :: Map.HashMap RoomId (Room IO)
  }

initialRegistryState :: RegistryState
initialRegistryState = RegistryState 
  {
    rooms = Map.empty
  }

newRegistry :: IO (Registry IO)
newRegistry = do
  stateVar <- newMVar initialRegistryState
  return $ Registry
    {
      numRooms   =
        Map.size       . rooms <$> readMVar stateVar
    , getRoom    = \rId ->
        Map.lookup rId . rooms <$> readMVar stateVar
    , createRoom = do
        rId       <- toText <$> nextRandom
        room      <- newRoom rId $ RegistryManage {selfDestructCallback=deleteRoomImpl stateVar rId}
        roomCount <- modifyMVar stateVar $ \st -> do
          let rooms' = Map.insert rId room $ rooms st
          return (st{rooms =  rooms'}, Map.size rooms')
        createDirectoryIfMissing True ("./rooms/" ++ unpack rId)
        print $ show roomCount ++ " rooms now after creating room " ++ unpack rId
        return rId
    , deleteRoom = deleteRoomImpl stateVar
    }

deleteRoomImpl :: MVar RegistryState -> RoomId -> IO ()
deleteRoomImpl stateVar rId = do
  roomCount <- modifyMVar stateVar $ \st -> do
    let rooms' = Map.delete rId $ rooms st
    return (st{rooms = rooms'}, Map.size rooms')
  removePathForcibly ("./rooms/" ++ unpack rId)
  print $ show roomCount ++ " rooms left after deleting room " ++ unpack rId
