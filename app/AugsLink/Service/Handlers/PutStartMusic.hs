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
startHandler rr rId uId = liftIO $ do
  r <- getRoom rr rId
  let room = case r of
               Just rm -> rm
               Nothing -> error "Room does not exist"

  startMusic room uId
    
  return NoContent
