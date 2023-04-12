{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Core.FFMpeg 
  (
    ffmpegConvertAudio
  , ffprobeAudio
  , FFProbeData (..)
  ) where

import Data.Aeson
import Data.Map
import Data.Text
import System.Process

import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy.Char8 as LBS

type FFMpegExecutableDir = FilePath
type FFProbeExecutableDir = FilePath

data FFProbeStream = Stream 
  {
    streamIndex     :: Maybe Int
  , codecName       :: Maybe Text
  , codecLongName   :: Maybe Text 
  , codecType       :: Maybe Text
  , codecTimeBase   :: Maybe Text
  , codecTagString  :: Maybe Text
  , codecTag        :: Maybe Text
  , width           :: Maybe Int
  , height          :: Maybe Int
  , hasBFrames      :: Maybe Int
  , pixelFormat     :: Maybe Text
  , level           :: Maybe Int
  , isAvc           :: Maybe Int
  , nalLengthSize   :: Maybe Int
  , rFrameRate      :: Maybe Text
  , avgFrameRate    :: Maybe Text
  , timeBase        :: Maybe Text
  , streamStartTime :: Maybe Double
  , streamDuration  :: Maybe Double
  , streamBitRate   :: Maybe Int
  , nbFrames        :: Maybe Int
  , streamTags      :: Maybe (Map String String)
  } deriving Show

instance FromJSON FFProbeStream where
  parseJSON = withObject "FFProbeStream" $ \o -> do
    idx           <- o .:? "index"
    codec         <- o .:? "codec_name"
    codecLong     <- o .:? "codec_long_name"
    codecType     <- o .:? "codec_type"
    codecTimeBase <- o .:? "codec_time_base"
    codecTagStr   <- o .:? "codec_tag_string"
    codecTag      <- o .:? "codec_tag"
    width         <- o .:? "width"
    height        <- o .:? "height"
    hasBFrames    <- o .:? "has_b_frames"
    pixFmt        <- o .:? "pix_fmt"
    lvl           <- o .:? "level"
    isAvc         <- o .:? "is_avc"
    nalLengthSize <- o .:? "nal_length_size"
    rFrameRate    <- o .:? "r_frame_rate"
    avgFrameRate  <- o .:? "avg_frame_rate"
    timeBase      <- o .:? "time_base"
    startTime     <- o .:? "start_time"
    duration      <- o .:? "duration"
    bitRate       <- o .:? "bit_rate"
    nbFrames      <- o .:? "nb_frames"
    tags          <- o .:? "tags" .!= Nothing
    return Stream
      { 
        streamIndex     = idx
      , codecName       = codec
      , codecLongName   = codecLong
      , codecType       = codecType
      , codecTimeBase   = codecTimeBase
      , codecTagString  = codecTagStr
      , codecTag        = codecTag
      , width           = width
      , height          = height
      , hasBFrames      = hasBFrames
      , pixelFormat     = pixFmt
      , level           = lvl
      , isAvc           = isAvc
      , nalLengthSize   = nalLengthSize
      , rFrameRate      = rFrameRate
      , avgFrameRate    = avgFrameRate
      , timeBase        = timeBase
      , streamStartTime = read . T.unpack <$> startTime
      , streamDuration  = read . T.unpack <$> duration
      , streamBitRate   = bitRate
      , nbFrames        = nbFrames
      , streamTags      = tags
      }

data FFProbeFormat = Format
  {
    fileName        :: Maybe Text
  , nbStreams       :: Maybe Int
  , formatName      :: Maybe Text
  , formatLongName  :: Maybe Text
  , formatStartTime :: Maybe Double
  , formatDuration  :: Maybe Double
  , formatSize      :: Maybe Int
  , formatBitRate   :: Maybe Int
  , formatTags      :: Maybe (Map String String)
  } deriving Show


instance FromJSON FFProbeFormat where
  parseJSON = withObject "FFProbeFormat" $ \o -> do
    fileName        <- o .: "filename"
    nbStreams       <- o .: "nb_streams"
    formatName      <- o .: "format_name"
    formatLongName  <- o .: "format_long_name"
    formatStartTime <- o .: "start_time"
    formatDuration  <- o .: "duration"
    formatSize      <- o .: "size"
    formatBitRate   <- o .: "bit_rate"
    formatTags      <- o .:? "tags" .!= Nothing
    return $ Format
      {
        fileName        = fileName
      , nbStreams       = nbStreams
      , formatName      = formatName
      , formatLongName  = formatLongName
      , formatStartTime = formatStartTime
      , formatDuration  = formatDuration
      , formatSize      = formatSize
      , formatBitRate   = formatBitRate
      , formatTags      = formatTags
      }


data FFProbeData = FFProbeData {
  streams :: [FFProbeStream]
, format  :: FFProbeFormat
} deriving Show

instance FromJSON FFProbeData where
  parseJSON = withObject "FFProbeData" $ \o -> do
    streams <- o .: "streams"
    format  <- o .: "format"
    return $ FFProbeData
      {
        streams = streams
      , format  = format
      }

ffprobeJsonOutArgs :: FilePath -> [String]
ffprobeJsonOutArgs filePath = 
  [
    "-v"           , "quiet"
  , "-print_format", "json"
  , "-show_format" , "-show_streams"
  , filePath
  ]

ffmpegConvertAudio :: FFMpegExecutableDir -> FilePath -> FilePath -> IO ()
ffmpegConvertAudio ffmpeg inputFile outputFile = do 
  callProcess ffmpeg ["-i", inputFile, outputFile]

ffprobeAudio :: FFProbeExecutableDir -> FilePath -> IO FFProbeData
ffprobeAudio ffprobe filePath = do
  jsonResponse <- readProcess ffprobe (ffprobeJsonOutArgs filePath) ""
  let result   = eitherDecode (LBS.pack jsonResponse)
  case result of
    Left err    -> 
      error $ "Error probing audio with ffprobe: " ++ err
    Right value -> 
      return value

