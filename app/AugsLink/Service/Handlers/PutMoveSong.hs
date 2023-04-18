{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Handlers.PutMoveSong
  (
    moveHandler
  ) where 

import Data.Text
import Control.Monad.IO.Class
import Servant

import AugsLink.Core.API

moveHandler :: Registry IO -> RoomId -> UserId -> SongId -> Priority -> Handler Text
moveHandler rr rId uId sId p = liftIO $ do
  r <- getRoom rr rId
  let room = case r of
               Just rm -> rm
               Nothing -> error "Room does not exist"

  u <- getUser room uId
  let user = case u of
               Just us -> us
               Nothing -> error "User does not exist"
  
  moveSong user sId p

  return sId
