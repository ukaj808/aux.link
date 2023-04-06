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

type instance Connection IO = WS.PendingConnection
type SongQueue = Heap.Heap (Heap.Entry Int Song)
data UserState = UserState
  {
    userData        :: RoomUser
  , userQueue       :: SongQueue
  , creator         :: Bool
  }

newUser :: UserId -> Bool -> IO (User IO)
newUser uId createdRoom = do
  uName <- return uId
  stateVar <- newMVar $ UserState (RoomUser uId uName) Heap.empty createdRoom
  return $ User {
    enqueueSong = enqueueSongImpl stateVar
  , getRoomUser = userData <$> readMVar stateVar
  , removeSong  = removeSongImpl stateVar
  , moveSong    = moveSongImpl stateVar
  , isCreator   = creator <$> readMVar stateVar
  , dequeueSong    = nextSongImpl stateVar
  }

nextSongImpl :: MVar UserState -> IO (Maybe Song)
nextSongImpl stateVar = do
 modifyMVar stateVar $ \st -> do
   let poll = Heap.uncons (userQueue st)
   case poll of 
     Just (e, q) -> 
       return (st{userQueue=q}, Just $ Heap.payload e)
     Nothing     -> 
       return (st, Nothing)

   

enqueueSongImpl :: MVar UserState -> SongInfo -> Priority -> IO SongId
enqueueSongImpl stateVar sInfo p = do
  sId <- toText <$> nextRandom
  modifyMVar_ stateVar $ \st -> do
    return $ modQueue st (Heap.insert (Heap.Entry p (Song sId sInfo)))
  return sId

removeSongImpl :: MVar UserState -> SongId -> IO ()
removeSongImpl stateVar sId = do
  modifyMVar_ stateVar $ \st -> do
    return $ modQueue st (Heap.filter (not . entryIsSong sId))

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
