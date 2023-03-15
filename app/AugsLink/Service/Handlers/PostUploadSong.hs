{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Handlers.PostUploadSong where 

import Servant
import qualified Data.Text as T
import Servant.Multipart
import AugsLink.Core.API (Registry, getRoom, uploadSong, SongFile)
import Control.Monad.Cont (liftIO)
import Data.UUID (fromText)

type instance SongFile IO = MultipartData Mem

upload :: Registry IO -> T.Text -> T.Text -> MultipartData Mem -> Handler T.Text
upload rr exRoomId exSongId file = liftIO $ do

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
