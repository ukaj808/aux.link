module AugsLink.Core.User
  ( 
    newUser 
  ) where


import Control.Concurrent
import Data.UUID.V4
import Data.UUID

import qualified Data.Heap         as Heap

import AugsLink.Core.API

type SongQueue = Heap.Heap (Heap.Entry Int Song)
data UserState = UserState
  {
    userData        :: RoomUser
  , userQueue       :: SongQueue
  }

newUser :: IO (User IO)
newUser = do
  uId <- toText <$> nextRandom
  uName <- return uId
  stateVar <- newMVar $ UserState (RoomUser uId uName) Heap.empty
  return $ User {
    enqueueSong = enqueueSongImpl stateVar
  , getRoomUser = userData <$> readMVar stateVar
  , getNextSong = getNextSongImpl stateVar
  , removeSong  = removeSongImpl stateVar
  , moveSong    = moveSongImpl stateVar
  }

enqueueSongImpl :: MVar UserState -> SongInfo -> Priority -> IO SongId
enqueueSongImpl stateVar sInfo p = do
  sId <- toText <$> nextRandom
  modifyMVar_ stateVar $ \st -> do
    return $ modQueue st (Heap.insert (Heap.Entry p (Song sId sInfo)))
  return sId

getNextSongImpl :: MVar UserState -> IO Song
getNextSongImpl stateVar = modifyMVar stateVar $ \st -> do
    let nextEntry = case Heap.uncons $ userQueue st of
                     Just nxt -> nxt
                     Nothing -> error "User queue is empty"
    let nxtSong = Heap.payload $ fst nextEntry
    let q' = snd nextEntry
    return (st{userQueue=q'}, nxtSong) 
      
removeSongImpl :: MVar UserState -> SongId -> IO ()
removeSongImpl stateVar sId = do
  modifyMVar_ stateVar $ \st -> do
    return $ modQueue st (Heap.filter (not . entryIsSong sId))

moveSongImpl :: MVar UserState -> SongId -> Priority -> IO ()
moveSongImpl stateVar sId p = do
  modifyMVar_ stateVar $ \st -> do
    return $ modQueue st (Heap.filter (not . entryIsSong sId))

modQueue :: UserState -> (SongQueue -> SongQueue) -> UserState
modQueue st@(UserState _ q) f = st{userQueue = f q}

entryIsSong :: SongId -> Heap.Entry a Song -> Bool
entryIsSong sId (Heap.Entry _ (Song sId' _)) = sId == sId'
