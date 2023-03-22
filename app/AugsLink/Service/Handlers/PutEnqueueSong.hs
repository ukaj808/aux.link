{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Handlers.PutEnqueueSong
  (
    enqueue
  ) where 

import Data.Text
import Data.UUID
import Control.Monad.IO.Class
import Servant
import Servant.Multipart

import AugsLink.Core.API

type instance SongFile IO = MultipartData Mem

enqueue :: Registry IO -> Text -> EnqueueSongRequest -> Handler Text
upload rr exRoomId req = liftIO $ do
  let rId = case fromText exRoomId of
              Just roomId -> roomId
              Nothing -> error "Invalid unique id"
  let sId = case fromText exSongId of
              Just songId -> songId
              Nothing -> error "Invalid unique id"
  r <- getRoom rr rId
  let room = case r of
               Just rm -> rm
               Nothing -> error "Room does not exist"
  uploadSong room sId file
  return "success"
