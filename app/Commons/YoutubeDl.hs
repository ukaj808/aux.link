{-# LANGUAGE OverloadedStrings #-}
module Commons.YoutubeDl
  (
    isValidUrl,
    ytdlp
  ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BLC

import System.Process
import Data.Aeson

type YtdlpExecutable = FilePath

isValidUrl :: T.Text -> IO Bool
isValidUrl = undefined

-- Args
--  Path to ytdlp executable -> Output directory -> Youtube URL
-- Output 
-- (Output file path, YtdlpOutput)
-- e.g. ("./output/songname.mp4", {...})
ytdlp :: YtdlpExecutable -> FilePath -> String -> IO (FilePath, YtdlpOutput)
ytdlp exec out url = do
  let outFilePath = ytdlpOutFilePath out
  jsonOutput <- readProcess exec (outputTemplateArg outFilePath ++ [fullJsonDumpArg, url]) ""
  let result = eitherDecode (BLC.pack jsonOutput) :: Either String YtdlpOutput
  case result of
    Left err -> error err
    Right ytdlpOut -> return (outFilePath, ytdlpOut)


ytdlTemplate :: String
ytdlTemplate = "%(title)s.%(ext)s"

fullJsonDumpArg :: String
fullJsonDumpArg = "-J"

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