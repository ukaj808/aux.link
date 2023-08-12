{-# LANGUAGE OverloadedStrings #-}
module Commons.FFMpeg
  (
    convertToWav
  , ffprobe
  , FFProbeData   (..)
  , FFProbeFormat (..)
  ) where

import Data.Aeson
import Data.Map
import Data.Text
import System.Process

import qualified Data.ByteString.Lazy.Char8 as BLC
import Control.Exception
import System.Directory



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
      , width           = read . unpack <$> width
      , height          = read . unpack <$> height
      , hasBFrames      = read . unpack <$> hasBFrames
      , pixelFormat     = pixFmt
      , level           = read . unpack <$> lvl
      , isAvc           = read . unpack <$> isAvc
      , nalLengthSize   = read . unpack <$> nalLengthSize
      , rFrameRate      = rFrameRate
      , avgFrameRate    = avgFrameRate
      , timeBase        = timeBase
      , streamStartTime = read . unpack <$> startTime
      , streamDuration  = read . unpack <$> duration
      , streamBitRate   = read . unpack <$> bitRate
      , nbFrames        = read . unpack <$> nbFrames
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
      , formatStartTime = read . unpack <$> formatStartTime
      , formatDuration  = read . unpack <$> formatDuration
      , formatSize      = read . unpack <$> formatSize
      , formatBitRate   = read . unpack <$> formatBitRate
      , formatTags      = formatTags
      }


data FFProbeData = FFProbeData {
  probedStreams :: [FFProbeStream]
, probedFormat  :: FFProbeFormat
} deriving Show

instance FromJSON FFProbeData where
  parseJSON = withObject "FFProbeData" $ \o -> do
    streams <- o .: "streams"
    format  <- o .: "format"
    return $ FFProbeData
      {
        probedStreams = streams
      , probedFormat  = format
      }

ffprobeJsonOutArgs :: FilePath -> [String]
ffprobeJsonOutArgs filePath = 
  [
    "-v"           , "quiet"
  , "-print_format", "json"
  , "-show_format" , "-show_streams"
  , filePath
  ]

ffprobe :: FilePath -> FilePath -> IO FFProbeData
ffprobe exec filePath = do
  jsonResponse <- readProcess exec (ffprobeJsonOutArgs filePath) ""
  let result = eitherDecode (BLC.pack jsonResponse) :: Either String FFProbeData
  case result of
    Left err    -> error $ "Error probing audio with ffprobe: " ++ err
    Right value -> return value

convertToWav :: FilePath -> FilePath -> String -> String -> IO FilePath
convertToWav ffmpeg dir name ext = do
  let i = dir ++ "/" ++ name ++ ext
  let o = dir ++ "/" ++ name ++ ".wav"
  bracket 
    (return ()) 
    (\_ -> removeFile i) $ 
    \_ -> do
    callProcess ffmpeg ["-i", i, "-ac", "2", "-ar", "48000", "-f", "wav", "-acodec", "pcm_f32le" , o]
    return o