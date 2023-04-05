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

  u <- getUser room uId
  let user = case u of
               Just us -> us
               Nothing -> error "User does not exist"
  
  stopListenToMusic user

  return "stop"
