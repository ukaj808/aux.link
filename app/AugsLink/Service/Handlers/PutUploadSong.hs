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

type instance SongFile IO = MultipartData Tmp

uploadHandler :: Registry IO -> RoomId -> UserId -> MultipartData Tmp -> Handler Text
uploadHandler rr rId uId file = liftIO $ do
  r <- getRoom rr rId
  let room = case r of
               Just rm -> rm
               Nothing -> error "Room does not exist"

  uploadSong room uId $ DirectFileUpload file
  return "success"
