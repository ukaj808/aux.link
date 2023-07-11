{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Handlers.PutStartMusic
  (
    startHandler
  ) where 

import Data.Text
import Control.Monad.IO.Class
import Servant

import AugsLink.Core.API

startHandler :: Registry IO -> RoomId -> UserId -> Handler NoContent
startHandler rr rId uId = do
  r <- liftIO $ getRoom rr rId
  let room = case r of
               Just rm -> rm
               Nothing -> error "Room does not exist"

  success <- liftIO $ startMusic room uId
    
  if success
    then return NoContent
    else
      throwError $ err401 { errBody = "You are not the creator of this room" }