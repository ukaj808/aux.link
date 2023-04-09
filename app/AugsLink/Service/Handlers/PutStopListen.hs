{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Handlers.PutStopListen
  (
    stopListenHandler
  ) where 

import Data.Text
import Control.Monad.IO.Class
import Servant

import AugsLink.Core.API

stopListenHandler :: Registry IO -> RoomId -> UserId -> Handler Text
stopListenHandler rr rId uId = liftIO $ do
  r <- getRoom rr rId
  let room = case r of
               Just rm -> rm
               Nothing -> error "Room does not exist"

  m <- getMusic room

  u <- getUser room uId
  let user = case u of
               Just us -> us
               Nothing -> error "Room does not exist"
  
  stopListening m room user

  return "stop"
