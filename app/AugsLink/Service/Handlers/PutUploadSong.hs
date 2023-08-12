{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Handlers.PutUploadSong
  (
    uploadHandler
  ) where

import Control.Monad.Cont
import Servant
import Servant.Multipart
import System.Directory
import System.IO.Temp

import qualified Data.Text as T

import AugsLink.Core.API
import Commons.YtDlp

uploadHandler :: Registry IO -> RoomId -> Maybe UserId -> MultipartData Tmp -> Handler NoContent
uploadHandler rr rId hUid mlt = do
  case hUid of
    Nothing -> throwError err401 { errBody = "No User Id provided" }
    Just uId -> do
      maybeRoom <- liftIO $ getRoom rr rId
      case maybeRoom of
        Nothing -> throwError err404
        Just room -> do
          let parseFile = lookupFile "file" mlt
          let parseUrl = lookupInput "url" mlt

          result <- case (parseFile, parseUrl) of
            (Left _, Left _) -> throwError err400
            (Right _, Right _) -> throwError err400
            (Right fileData, Left _) -> do
              liftIO $ uploadSong room uId $
                Upload {uploadName=fdFileName fileData, uploadTmp=fdPayload fileData}
            (Left _, Right url) -> do
              roomPath <- liftIO $ getRoomPath room
              withTempDirectory  roomPath "upload" $ \tmpDir -> do
                (tmpPath, metadata) <- liftIO $ ytdlpDownload "yt-dlp" tmpDir url
                liftIO $ uploadSong room uId $
                  Upload {uploadName=ytdlpTitle metadata, uploadTmp=tmpPath}

          if result
            then return NoContent
            else throwError err409
