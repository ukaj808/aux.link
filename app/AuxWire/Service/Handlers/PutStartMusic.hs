{-# LANGUAGE OverloadedStrings #-}
module AuxWire.Service.Handlers.PutStartMusic
  (
    startHandler
  ) where 

import Control.Monad.IO.Class
import Servant

import AuxWire.Core.API

startHandler :: Registry IO -> RoomId -> Maybe UserId -> Handler NoContent
startHandler rr rId hUid = do
  case hUid of 
    Nothing -> throwError $ err401 { errBody = "No User Id provided" }
    Just uId -> do
      maybeRoom <- liftIO $ getRoom rr rId
      case maybeRoom of
        Nothing   -> throwError $ err404 { errBody = "Room not found" }
        Just room -> do
          result <- liftIO $ startMusic room uId
          case result of
            StartMusicSuccess -> return NoContent
            NotCreator        -> throwError $ err401 { errBody = "You are not the creator of this room"  }
            RoomStillCreating -> throwError $ err409 { errBody = "The room is still being created"       }
            AlreadyRunning    -> throwError $ err409 { errBody = "The music streamer is already running" }
