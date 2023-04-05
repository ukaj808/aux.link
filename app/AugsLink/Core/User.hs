module AugsLink.Core.User
  ( 
    newUser 
  ) where


import Control.Concurrent
import Data.UUID.V4
import Data.UUID

import qualified Data.Heap         as Heap
import qualified Network.WebSockets as WS

import AugsLink.Core.API
import AugsLink.Core.Shared

type instance Connection IO = WS.PendingConnection
type SongQueue = Heap.Heap (Heap.Entry Int Song)
data UserState = UserState
  {
    userData        :: RoomUser
  , userQueue       :: SongQueue
  , roomManage      :: RoomManage
  }

newUser :: UserId -> RoomManage -> IO (User IO)
newUser uId rm = do
  uName <- return uId
  stateVar <- newMVar $ UserState (RoomUser uId uName) Heap.empty rm
  return $ User {
    enqueueSong = enqueueSongImpl stateVar
  , getRoomUser = userData <$> readMVar stateVar
  , getNextSong = getNextSongImpl stateVar
  , removeSong  = removeSongImpl stateVar
  , moveSong    = moveSongImpl stateVar
  , startMusic  = startMusicImpl stateVar
  , stopListenToMusic = stopListenToMusicImpl stateVar
  , listenToMusic = listenToMusicImpl stateVar
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

startMusicImpl :: MVar UserState -> IO ()
startMusicImpl stateVar = do
  st <- readMVar stateVar
  either error id 
    (startMusicCallback (roomManage st))

stopListenToMusicImpl :: MVar UserState -> IO ()
stopListenToMusicImpl stateVar = do
  st <- readMVar stateVar
  stopListenToMusicCallback (roomManage st)

listenToMusicImpl :: MVar UserState -> Connection IO ->IO ()
listenToMusicImpl stateVar pend = do
  st <- readMVar stateVar
  listenToMusicCallback (roomManage st) pend

moveSongImpl :: MVar UserState -> SongId -> Priority -> IO ()
moveSongImpl stateVar sId p = do
  modifyMVar_ stateVar $ \st -> do
    let s =  Heap.payload $ Heap.minimum $ Heap.filter (entryIsSong sId) (userQueue st)
    let s' = Heap.Entry p $ Song (songId s) (songInfo s)
    return $ modQueue st (Heap.insert s' . Heap.filter (not . entryIsSong sId))

modQueue :: UserState -> (SongQueue -> SongQueue) -> UserState
modQueue st@(UserState _ q _) f = st{userQueue = f q}

entryIsSong :: SongId -> Heap.Entry a Song -> Bool
entryIsSong sId (Heap.Entry _ (Song sId' _)) = sId == sId'
