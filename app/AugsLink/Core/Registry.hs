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
import System.FilePath

data RegistryState = RegistryState
  {
    rooms :: Map.HashMap RoomId (Room IO)
  , roomsPath :: FilePath
  }

initialRegistryState :: FilePath -> RegistryState
initialRegistryState roomsPath = RegistryState 
  {
    rooms = Map.empty
  , roomsPath = roomsPath
  }

newRegistry :: FilePath -> IO (Registry IO)
newRegistry roomsPath = do
  stateVar <- newMVar $ initialRegistryState roomsPath
  return $ Registry
    {
      numRooms   =
        Map.size       . rooms <$> readMVar stateVar
    , getRoom    = \rId ->
        Map.lookup rId . rooms <$> readMVar stateVar
    , createRoom = do
        rId       <- toText <$> nextRandom
        let roomPath = roomsPath </> unpack rId
        createDirectory roomPath
        room      <- newRoom rId roomPath RegistryManage {selfDestructCallback=deleteRoomImpl stateVar rId}
        roomCount <- modifyMVar stateVar $ \st -> do
          let rooms' = Map.insert rId room $ rooms st
          return (st{rooms =  rooms'}, Map.size rooms')
        print $ show roomCount ++ " rooms now after creating room " ++ unpack rId
        return rId
    , deleteRoom = deleteRoomImpl stateVar
    }

deleteRoomImpl :: MVar RegistryState -> RoomId -> IO ()
deleteRoomImpl stateVar rId = do
  roomCount <- modifyMVar stateVar $ \st -> do
    let rooms' = Map.delete rId $ rooms st
    removeDirectoryRecursive (roomsPath st </> unpack rId)
    return (st{rooms = rooms'}, Map.size rooms')
  print $ show roomCount ++ " rooms left after deleting room " ++ unpack rId
