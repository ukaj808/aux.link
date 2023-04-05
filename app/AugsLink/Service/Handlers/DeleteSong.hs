{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Handlers.DeleteSong
  (
    removeHandler
  ) where 

import Data.Text
import Control.Monad.IO.Class
import Servant

import AugsLink.Core.API

removeHandler :: Registry IO -> RoomId -> UserId -> SongId -> Handler Text
removeHandler rr rId uId sId = liftIO $ do
  r <- getRoom rr rId
  let room = case r of
               Just rm -> rm
               Nothing -> error "Room does not exist"

  u <- getUser room uId
  let user = case u of
               Just us -> us
               Nothing -> error "User does not exist"
  
  removeSong user sId

  return sId
