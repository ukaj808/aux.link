{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Handlers.PutEnqueueSong
  (
    enqueueHandler
  ) where 

import Data.Text
import Control.Monad.IO.Class
import Servant
import Servant.Multipart

import AugsLink.Core.API
import AugsLink.Service.API

type instance SongFile IO = MultipartData Mem

enqueueHandler :: Registry IO -> RoomId -> UserId -> EnqueueSongRequest -> Handler Text
enqueueHandler rr rId uId req = liftIO $ do
  r <- getRoom rr rId
  let room = case r of
               Just rm -> rm
               Nothing -> error "Room does not exist"

  u <- getUser room uId
  let user = case u of
               Just us -> us
               Nothing -> error "User does not exist"
  
  enqueueSong user (song req) (priority req)
