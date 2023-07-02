{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Handlers.PutUploadSong
  (
    uploadHandler
  ) where 

import Control.Monad.Cont
import Servant
import Servant.Multipart

import qualified Data.Text as T

import AugsLink.Core.API

uploadHandler :: Registry IO -> RoomId -> UserId -> MultipartData Tmp -> Handler NoContent
uploadHandler rr rId uId mlt = do
  maybeRoom <- liftIO $ getRoom rr rId
  case maybeRoom of
    Nothing -> throwError err404
    Just room -> do
      -- check if file in multipart data
      -- if not, then check if url in multipart data
      -- if not, then return 400
      let parseFile = lookupFile "file" mlt
      let parseUrl = lookupInput "url" mlt

      case (parseFile, parseUrl) of
        (Left _, Left _) -> throwError err400
        (Right _, Right _) -> throwError err400
        (Right fileData, Left _) -> do
          liftIO $ uploadSong room uId $ DirectFileUpload (T.unpack $ fdFileName fileData, fdPayload fileData)
        (Left _, Right input) -> do
          liftIO $ uploadSong room uId $ UrlScrapeUpload input
      
      return NoContent
