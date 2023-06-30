{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Commons.YoutubeDl
  (
    ytdlpValid,
    ytdlpDownload
  ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BLC

import System.Process
import Data.Aeson
import Control.Exception

type YtdlpExecutable = FilePath

ytdlpValid :: YtdlpExecutable -> String -> IO Bool
ytdlpValid exec url = -- validates the url by trying to download it
  catch (do
    _ <- ytdlp exec [skipDownloadArg] url
    return True
  ) (\(_ :: IOError) -> return False)

ytdlpDownload :: YtdlpExecutable -> FilePath -> String -> IO (FilePath, YtdlpOutput)
ytdlpDownload exec out url = do
  let outFilePath =  ytdlpOutFilePath out
  let outArg      =  outputTemplateArg outFilePath
  ytdlOutput      <- ytdlp exec outArg url
  return             (outFilePath, ytdlOutput)

ytdlp :: YtdlpExecutable -> [String] -> String -> IO YtdlpOutput
ytdlp exec args url = do
  jsonOutput <- readProcess exec (args ++ [fullJsonDumpArg, url]) ""
  let result = eitherDecode (BLC.pack jsonOutput) :: Either String YtdlpOutput
  case result of
    Left err -> error err
    Right ytdlpOut -> return ytdlpOut


ytdlTemplate :: String
ytdlTemplate = "%(title)s.%(ext)s"

fullJsonDumpArg :: String
fullJsonDumpArg = "-J"

skipDownloadArg :: String
skipDownloadArg = "--skip-download"

outputTemplateArg :: FilePath -> [String]
outputTemplateArg path = ["-o", path]

ytdlpOutFilePath :: FilePath -> FilePath
ytdlpOutFilePath [] = ytdlTemplate
ytdlpOutFilePath dir
  | last dir == '/' = dir ++ ytdlTemplate
  | otherwise = '/' : dir ++ ytdlTemplate


data YtdlpOutput = YtdlpOutput
  {
    ytdlId :: T.Text,
    ytdlTitle :: T.Text,
    ytdlThumbnail :: T.Text,
    ytdlDescription :: T.Text,
    ytdlUploader :: T.Text,
    ytdlDuration :: Int,
    ytdlChannel :: T.Text,
    ytdlFullTitle :: T.Text
  }

instance FromJSON YtdlpOutput where
  parseJSON = withObject "YtdlpOutput" $ \o -> do
    ytdlpId <- o .: "id"
    ytdlpTitle <- o .: "title"
    ytdlpThumbnail <- o .: "thumbnail"
    ytdlpDescription <- o .: "description"
    ytdlpUploader <- o .: "uploader"
    ytdlpDuration <- o .: "duration"
    ytdlpChannel <- o .: "channel"
    ytdlpFullTitle <- o .: "fulltitle"
    return YtdlpOutput
      {
        ytdlId = ytdlpId
      , ytdlTitle = ytdlpTitle
      , ytdlThumbnail = ytdlpThumbnail
      , ytdlDescription = ytdlpDescription
      , ytdlUploader = ytdlpUploader
      , ytdlDuration = ytdlpDuration
      , ytdlChannel = ytdlpChannel
      , ytdlFullTitle = ytdlpFullTitle
      }