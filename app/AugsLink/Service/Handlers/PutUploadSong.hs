{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Handlers.PutUploadSong
  (
    uploadHandler
  ) where 

import Data.Text
import Control.Monad.IO.Class
import Servant
import Servant.Multipart

import AugsLink.Core.API

type instance SongFile IO = MultipartData Mem

uploadHandler :: Registry IO -> RoomId -> UserId ->SongId -> MultipartData Mem -> Handler Text
uploadHandler rr rId uId sId file = liftIO $ do
  r <- getRoom rr rId
  let room = case r of
               Just rm -> rm
               Nothing -> error "Room does not exist"

  u <- getUser room uId
  let user = case u of
               Just us -> us
               Nothing -> error "User does not exist"
  uploadSong user sId file
  return "success"
