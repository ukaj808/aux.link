{-# LANGUAGE OverloadedStrings #-}
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
import System.Directory (doesFileExist)
import Data.Text
import qualified Data.ByteString.Lazy as LBS
import Servant.Multipart

type instance Connection IO = WS.PendingConnection
type instance SongFile IO       = MultipartData Mem
type SongQueue = Heap.Heap (Heap.Entry Int SongId)
data UserState = UserState
  {
    userData        :: RoomUser
  , userQueue       :: SongQueue
  }

newUser :: RoomId -> UserId -> IO (User IO)
newUser rId uId = do
  let uName = pack $ show uId
  stateVar <- newMVar $ UserState (RoomUser uId uName) Heap.empty
  return $ User {
    enqueueSong = enqueueSongImpl stateVar
  , getRoomUser = userData <$> readMVar stateVar
  , removeSong  = removeSongImpl stateVar
  , moveSong    = moveSongImpl stateVar
  , dequeueSong = dequeueSongImpl stateVar
  , uploadSong  = uploadSongImpl rId
  }

dequeueSongImpl :: MVar UserState -> IO (Either String (Maybe SongId))
dequeueSongImpl stateVar = do
 modifyMVar stateVar $ \st -> do
   let poll = Heap.uncons (userQueue st)
   case poll of
     Just (e, q) -> do
       let sId = Heap.payload e
       songUploaded <- wasSongUploaded sId
       case songUploaded of
         Left msg -> return (st{userQueue=q}, Left msg)
         Right _ -> return (st{userQueue=q}, Right $ Just sId)
     Nothing     ->
       return (st, Right Nothing)

wasSongUploaded :: SongId -> IO (Either String ())
wasSongUploaded sId = do
  songUploaded <- pollForFile "/uploads" (unpack sId ++ ".mp3") 5
  if songUploaded
  then return $ Right ()
  else return $ Left "Song wasnt uploaded"
  where
    pollForFile :: FilePath -> String -> Int -> IO Bool
    pollForFile dir fileName maxAttempts = pollForFile' 0
        where
          pollForFile' attempts
            | attempts >= maxAttempts = return False
            | otherwise = do
                fileFound <- doesFileExist (dir ++ "/" ++ fileName)
                if fileFound
                  then return True
                  else do
                    threadDelay 1000000 -- wait for 1 second
                    pollForFile' (attempts + 1)


enqueueSongImpl :: MVar UserState -> Priority -> IO SongId
enqueueSongImpl stateVar p = do
  sId <- toText <$> nextRandom
  modifyMVar_ stateVar $ \st -> do
    return $ modQueue st (Heap.insert (Heap.Entry p sId))
  return sId

removeSongImpl :: MVar UserState -> SongId -> IO ()
removeSongImpl stateVar sId = do
  modifyMVar_ stateVar $ \st -> do
    return $ modQueue st (Heap.filter (not . entryIsSong sId))

moveSongImpl :: MVar UserState -> SongId -> Priority -> IO ()
moveSongImpl stateVar sId p = do
  modifyMVar_ stateVar $ \st -> do
    let s' = Heap.Entry p sId
    return $ modQueue st (Heap.insert s' . Heap.filter (not . entryIsSong sId))

uploadSongImpl :: RoomId -> SongId -> SongFile IO -> IO ()
uploadSongImpl rId sId sFile = do
  let file = lookupFile "song" sFile
  either
    (error "No file present in request")
    (uploadSongToLocal rId sId) file

uploadSongToLocal :: RoomId -> SongId -> FileData Mem -> IO ()
uploadSongToLocal rId sId file = do
  let filePath = "./uploads/" ++ unpack rId ++ "/" ++ unpack sId
  fileExist <- doesFileExist filePath
  if fileExist
  then
   error "Song already uploaded"
  else do
    LBS.writeFile filePath (fdPayload file)

modQueue :: UserState -> (SongQueue -> SongQueue) -> UserState
modQueue st@(UserState _ q) f = st{userQueue = f q}

entryIsSong :: SongId -> Heap.Entry a SongId -> Bool
entryIsSong sId (Heap.Entry _ sId') = sId == sId'
