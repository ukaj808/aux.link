{-# LANGUAGE OverloadedStrings #-}
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
    userData        :: UserData --rename? too vague maybe (over encompassing...) maybe seperate out id from data and rename to info
  , userQueue       :: SongQueue
  }

newUser :: IO (User IO)
newUser = do
  uId <- nextRandom
  uName <- return $ toText uId
  stateVar <- newMVar $ UserState (UserData uId uName) Heap.empty
  return $ User {
    enqueueSong = enqueueSongImpl stateVar
  , getUserData = userData <$> readMVar stateVar
  , removeSong = removeSongImpl stateVar
  }

enqueueSongImpl :: MVar UserState -> SongInfo -> Priority -> IO SongId
enqueueSongImpl stateVar sInfo p = do
  sId <- nextRandom
  modifyMVar_ stateVar $ \st -> do
    return $ modQueue st (Heap.insert (Heap.Entry p (Song sId sInfo)))
  return sId

removeSongImpl :: MVar UserState -> SongId -> IO ()
removeSongImpl stateVar sId = do
  modifyMVar_ stateVar $ \st -> do
    return $ modQueue st (Heap.filter (not . entryIsSong sId))

modQueue :: UserState -> (SongQueue -> SongQueue) -> UserState
modQueue st@(UserState _ q) f = st{userQueue = f q}

entryIsSong :: SongId -> Heap.Entry a Song -> Bool
entryIsSong sId (Heap.Entry _ (Song sId' _)) = sId == sId'
