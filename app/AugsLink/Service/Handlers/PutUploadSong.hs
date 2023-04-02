{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Handlers.PutUploadSong
  (
    upload
  ) where 

import Data.Text
import Control.Monad.IO.Class
import Servant
import Servant.Multipart

import AugsLink.Core.API

type instance SongFile IO = MultipartData Mem

upload :: Registry IO -> RoomId -> SongId -> MultipartData Mem -> Handler Text
upload rr rId sId file = liftIO $ do
  r <- getRoom rr rId
  let room = case r of
               Just rm -> rm
               Nothing -> error "Room does not exist"
  uploadSong room sId file
  return "success"
