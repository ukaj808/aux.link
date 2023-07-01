{-# LANGUAGE OverloadedStrings #-}
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
type URL = T.Text

ytdlpValid :: YtdlpExecutable -> URL -> IO Bool
ytdlpValid exec url = -- validates the url by trying to download it
  catch (do
    _ <- ytdlp exec [skipDownloadArg] url True
    return True
  ) (\(e :: IOError) -> do
    print $ show e
    return False
    )

ytdlpDownload :: YtdlpExecutable -> FilePath -> URL -> IO (FilePath, YtdlpOutput)
ytdlpDownload exec out url = do
  ytdlOutput      <- ytdlp exec (forceOverwriteArg : ytdlpOutputArg out) url False
  return             (ytdlpTemplatePathReplaced out ytdlOutput, ytdlOutput)

ytdlp :: YtdlpExecutable -> [String] -> URL -> Bool -> IO YtdlpOutput
ytdlp exec args url sim = do
  let noSimArg' = if sim then [] else [noSimArg]
  jsonOutput <- readProcess exec (args ++ noSimArg' ++ [fullJsonDumpArg, (T.unpack url)]) ""
  let result = eitherDecode (BLC.pack jsonOutput) :: Either String YtdlpOutput
  case result of
    Left err -> error err
    Right ytdlpOut -> return ytdlpOut

-- Takes a directory path and returns the template string used in the youtube-dl command
ytdlpOutputArg :: FilePath -> [String]
ytdlpOutputArg = outputTemplateArg . ytdlpTemplatePath

ytdlpTemplatePathReplaced :: FilePath -> YtdlpOutput -> FilePath
ytdlpTemplatePathReplaced dir ytdlpOut = T.unpack $ T.replace "%(ext)s" (ytdlExt ytdlpOut) $ T.replace "%(title)s" (ytdlTitle ytdlpOut) (T.pack $ ytdlpTemplatePath dir)

ytdlpTemplatePath :: FilePath -> FilePath
ytdlpTemplatePath [] = ytdlTemplateStr
ytdlpTemplatePath dir
  | last dir == '/' = dir ++ ytdlTemplateStr
  | otherwise = '/' : dir ++ ytdlTemplateStr

ytdlTemplateStr :: String
ytdlTemplateStr = "%(title)s.%(ext)s"


forceOverwriteArg :: String
forceOverwriteArg = "--force-overwrites"

noSimArg :: String
noSimArg = "--no-simulate"

-- Should be used in conjuction with noSimArg if you want to download the video
-- otherwise it wont actually download the video; just simulate a download
fullJsonDumpArg :: String
fullJsonDumpArg = "-J"

skipDownloadArg :: String
skipDownloadArg = "--skip-download"

outputTemplateArg :: FilePath -> [String]
outputTemplateArg path = ["-o", path]


data YtdlpOutput = YtdlpOutput
  {
    ytdlId          :: T.Text,
    ytdlTitle       :: T.Text,
    ytdlThumbnail   :: T.Text,
    ytdlDescription :: T.Text,
    ytdlUploader    :: T.Text,
    ytdlChannel     :: T.Text,
    ytdlFullTitle   :: T.Text,
    ytdlExt         :: T.Text,
    ytdlDuration    :: Int
  } deriving (Show)

instance FromJSON YtdlpOutput where
  parseJSON = withObject "YtdlpOutput" $ \o -> do
    ytdlpId          <- o .: "id"
    ytdlpTitle       <- o .: "title"
    ytdlpThumbnail   <- o .: "thumbnail"
    ytdlpDescription <- o .: "description"
    ytdlpUploader    <- o .: "uploader"
    ytdlpDuration    <- o .: "duration"
    ytdlpChannel     <- o .: "channel"
    ytdlpFullTitle   <- o .: "fulltitle"
    ytdlpExt         <- o .: "ext"
    return YtdlpOutput
      {
        ytdlId          = ytdlpId
      , ytdlTitle       = ytdlpTitle
      , ytdlThumbnail   = ytdlpThumbnail
      , ytdlDescription = ytdlpDescription
      , ytdlUploader    = ytdlpUploader
      , ytdlDuration    = ytdlpDuration
      , ytdlChannel     = ytdlpChannel
      , ytdlFullTitle   = ytdlpFullTitle
      , ytdlExt        = ytdlpExt
      }