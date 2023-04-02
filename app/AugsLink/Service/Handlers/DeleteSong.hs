{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Handlers.DeleteSong
  (
    remove
  ) where 

import Data.Text
import Control.Monad.IO.Class
import Servant

import AugsLink.Core.API

remove :: Registry IO -> RoomId -> UserId -> SongId -> Handler Text
remove rr rId uId sId = liftIO $ do
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
