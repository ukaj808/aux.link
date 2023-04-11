{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Core.FFMpeg 
  (
    ffmpegConvertAudio
  , ffprobeAudio
  , FFProbeData (..)
  ) where

import System.Process
import Data.Text
import Data.Map
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as LBS

type FFMpegExecutableDir = FilePath
type FFProbeExecutableDir = FilePath

data FFProbeStream = Stream 
  {
    streamIndex :: Int
  , codecName :: Text
  , codecLongName :: Text 
  , codecType :: Text
  , codecTimeBase :: Text
  , codecTagString :: Text
  , codecTag :: Text
  , width :: Int
  , height :: Int
  , hasBFrames :: Int
  , pixelFormat :: Text
  , level :: Int
  , isAvc :: Int
  , nalLengthSize :: Int
  , rFrameRate :: Text
  , avgFrameRate :: Text
  , timeBase :: Text
  , streamStartTime :: Double
  , streamDuration :: Double
  , streamBitRate :: Int
  , nbFrames :: Int
  , streamTags :: Map String String
  } deriving Show

instance FromJSON FFProbeStream where
  parseJSON = withObject "FFProbeStream" $ \o -> do
    idx <- o .: "index"
    codec <- o .: "codec_name"
    codecLong <- o .: "codec_long_name"
    codecType <- o .: "codec_type"
    codecTimeBase <- o .: "codec_time_base"
    codecTagStr <- o .: "codec_tag_string"
    codecTag <- o .: "codec_tag"
    width <- o .: "width"
    height <- o .: "height"
    hasBFrames <- o .: "has_b_frames"
    pixFmt <- o .: "pix_fmt"
    lvl <- o .: "level"
    isAvc <- o .: "is_avc"
    nalLengthSize <- o .: "nal_length_size"
    rFrameRate <- o .: "r_frame_rate"
    avgFrameRate <- o .: "avg_frame_rate"
    timeBase <- o .: "time_base"
    startTime <- o .: "start_time"
    duration <- o .: "duration"
    bitRate <- o .: "bit_rate"
    nbFrames <- o .: "nb_frames"
    tags <- o .:? "tags" .!= Map.empty
    return Stream
      { streamIndex = idx
      , codecName = codec
      , codecLongName = codecLong
      , codecType = codecType
      , codecTimeBase = codecTimeBase
      , codecTagString = codecTagStr
      , codecTag = codecTag
      , width = width
      , height = height
      , hasBFrames = hasBFrames
      , pixelFormat = pixFmt
      , level = lvl
      , isAvc = isAvc
      , nalLengthSize = nalLengthSize
      , rFrameRate = rFrameRate
      , avgFrameRate = avgFrameRate
      , timeBase = timeBase
      , streamStartTime = read (T.unpack startTime) -- convert String to Double
      , streamDuration = read (T.unpack duration) -- convert String to Double
      , streamBitRate = bitRate
      , nbFrames = nbFrames
      , streamTags = tags
      }

data FFProbeFormat = Format
  {
    fileName :: Text
  , nbStreams :: Int
  , formatName :: Text
  , formatLongName :: Text
  , formatStartTime :: Double
  , formatDuration :: Double
  , size :: Int
  , formatBitRate :: Int
  , formatTags :: Map String String
  } deriving Show


instance FromJSON FFProbeFormat where
  parseJSON = withObject "FFProbeFormat" $ \o -> do
    formatFileName <- o .: "filename"
    formatNbStreams <- o .: "nb_streams"
    formatName <- o .: "format_name"
    formatLongName <- o .: "format_long_name"
    formatStartTime <- o .: "start_time"
    formatDuration <- o .: "duration"
    formatSize <- o .: "size"
    formatBitRate <- o .: "bit_rate"
    formatTags <- o .:? "tags" .!= Map.empty
    return $ Format formatFileName formatNbStreams formatName formatLongName formatStartTime formatDuration formatSize formatBitRate formatTags


data FFProbeData = FFProbeData {
  streams :: [FFProbeStream]
, format  :: FFProbeFormat
} deriving Show

instance FromJSON FFProbeData where
  parseJSON = withObject "FFProbeData" $ \o -> do
    streams <- o .: "streams"
    format <- o .: "format"
    return $ FFProbeData streams format

ffprobeJsonOutArgs :: FilePath -> [String]
ffprobeJsonOutArgs filePath = ["-v", "quiet", "-print_format", "json", "-show_format", "-show_streams", filePath]

ffmpegConvertAudio :: FFMpegExecutableDir -> FilePath -> FilePath -> IO ()
ffmpegConvertAudio ffmpeg inputFile outputFile = do 
  callProcess ffmpeg ["-i", inputFile, outputFile]

ffprobeAudio :: FFProbeExecutableDir -> FilePath -> IO FFProbeData
ffprobeAudio ffprobe filePath = do
  jsonResponse <- readProcess ffprobe (ffprobeJsonOutArgs filePath) ""
  let result = eitherDecode (LBS.pack jsonResponse)
  case result of
    Left err -> error $ "Error probing audio with ffprobe: " ++ err
    Right value -> return value

