{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Handlers.PutStartMusic
  (
    startHandler
  ) where 

import Data.Text
import Control.Monad.IO.Class
import Servant

import AugsLink.Core.API hiding (start)

startHandler :: Registry IO -> RoomId -> UserId -> Handler Text
startHandler rr rId uId = liftIO $ do
  r <- getRoom rr rId
  let room = case r of
               Just rm -> rm
               Nothing -> error "Room does not exist"

  u <- getUser room uId
  let user = case u of
               Just us -> us
               Nothing -> error "User does not exist"
  
  startMusic user

  return "start"
