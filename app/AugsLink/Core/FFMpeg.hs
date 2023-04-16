{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Core.FFMpeg 
  (
    ffprobe
  , FFProbeData (..)
  , formatDuration
  ) where

import Data.Aeson
import System.Process

import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map as Map
import Data.Int
import Data.Maybe

type FFProbeExecutableDir = FilePath

data FFProbeStream = Stream 
  {
    streamIndex     :: Maybe Int
  , codecName       :: Maybe T.Text
  , codecLongName   :: Maybe T.Text 
  , codecType       :: Maybe T.Text
  , codecTimeBase   :: Maybe T.Text
  , codecTagString  :: Maybe T.Text
  , codecTag        :: Maybe T.Text
  , channels        :: Maybe Int
  , width           :: Maybe Int
  , height          :: Maybe Int
  , hasBFrames      :: Maybe Int
  , pixelFormat     :: Maybe T.Text
  , level           :: Maybe Int
  , isAvc           :: Maybe Int
  , nalLengthSize   :: Maybe Int
  , rFrameRate      :: Maybe T.Text
  , avgFrameRate    :: Maybe T.Text
  , timeBase        :: Maybe T.Text
  , sampleRate      :: Maybe Int
  , streamStartTime :: Maybe Double
  , streamDuration  :: Maybe Double
  , streamBitRate   :: Maybe Int
  , nbFrames        :: Maybe Int
  , streamTags      :: Maybe (Map.Map String String)
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
    channels      <- o .:? "channels"
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
    sampleRate    <- o .:? "sample_rate"
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
      , channels        = channels
      , width           = read . T.unpack <$> width
      , height          = read . T.unpack <$> height
      , hasBFrames      = read . T.unpack <$> hasBFrames
      , pixelFormat     = pixFmt
      , level           = read . T.unpack <$> lvl
      , isAvc           = read . T.unpack <$> isAvc
      , nalLengthSize   = read . T.unpack <$> nalLengthSize
      , rFrameRate      = rFrameRate
      , avgFrameRate    = avgFrameRate
      , timeBase        = timeBase
      , sampleRate      = read . T.unpack <$> sampleRate
      , streamStartTime = read . T.unpack <$> startTime
      , streamDuration  = read . T.unpack <$> duration
      , streamBitRate   = read . T.unpack <$> bitRate
      , nbFrames        = read . T.unpack <$> nbFrames
      , streamTags      = tags
      }

data FFProbeFormat = Format
  {
    fileName        :: Maybe T.Text
  , nbStreams       :: Maybe Int
  , formatName      :: Maybe T.Text
  , formatLongName  :: Maybe T.Text
  , formatStartTime :: Maybe Double
  , formatDuration  :: Maybe Double
  , formatSize      :: Maybe Int
  , formatBitRate   :: Maybe Int
  , formatTags      :: Maybe (Map.Map String String)
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
      , formatStartTime = read . T.unpack <$> formatStartTime
      , formatDuration  = read . T.unpack <$> formatDuration
      , formatSize      = read . T.unpack <$> formatSize
      , formatBitRate   = read . T.unpack <$> formatBitRate
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

ffprobe :: FFProbeExecutableDir -> FilePath -> IO FFProbeData
ffprobe exec filePath = do
  jsonResponse <- readProcess exec (ffprobeJsonOutArgs filePath) ""
  let result   = eitherDecode (LBS.pack jsonResponse)
  case result of
    Left err    -> 
      error $ "Error probing audio with ffprobe: " ++ err
    Right value -> 
      return value


mp3FrameSize :: Int      -- ^ Bitrate (kbps)
             -> Int      -- ^ Sample rate (samples/sec)
             -> Int64      -- ^ Frame size (bytes)
mp3FrameSize bitrate sampleRate =
  let frameDuration = (1152 :: Double) -- MP3 frame duration in samples
      frameSize = fromIntegral bitrate * 1000 * frameDuration / fromIntegral sampleRate / 8
  in round frameSize

data AudioFormat = Raw | MP3

calculateChunkSize :: AudioFormat -- ^ Audio format
                   -> Int         -- ^ Bit depth (bits/sample) or bitrate (kbps) for compressed formats
                   -> Int         -- ^ Sample rate (samples/sec)
                   -> Double      -- ^ Duration of audio (seconds)
                   -> Int         -- ^ Number of channels
                   -> Int         -- ^ Desired duration of each chunk (milliseconds)
                   -> Int64       -- ^ Chunk size (bytes)
calculateChunkSize format bitDepthOrBitrate sampleRate duration numChannels chunkDurationMs =
  let totalBytes = case format of
                     Raw  -> fromIntegral bitDepthOrBitrate * fromIntegral sampleRate * fromIntegral numChannels * duration / 8
                     MP3  -> fromIntegral bitDepthOrBitrate * 1000 * duration / 8
      chunkDurationSec = fromIntegral chunkDurationMs / 1000
      baseChunkSize :: Int64 = round (totalBytes * chunkDurationSec / duration)
      adjustedChunkSize = case format of
                            Raw  -> baseChunkSize
                            MP3  -> let frameSize = mp3FrameSize bitDepthOrBitrate sampleRate
                                    in (baseChunkSize `div` frameSize) * frameSize
  in adjustedChunkSize


calculateChunkSizeFromFFProbeData :: Int -> FFProbeData -> Int64
calculateChunkSizeFromFFProbeData chunkDurationMs ffprobeData  =
  let audioStream = head $ filter (\stream -> codecType stream == Just "audio") (streams ffprobeData)
      audioFormat = case codecName audioStream of
                      Just "pcm_s16le" -> Raw
                      Just "mp3"       -> MP3
                      _                -> error "Unsupported audio format"
      bitDepthOrBitrate = fromMaybe 0 (streamBitRate audioStream)
      sampleRate' = fromMaybe 0 (sampleRate audioStream)
      duration = fromMaybe 0 (streamDuration audioStream)
      numChannels = fromMaybe 0 (channels audioStream)
  in calculateChunkSize audioFormat bitDepthOrBitrate sampleRate' duration numChannels chunkDurationMs

calc :: IO FFProbeData -> IO Int64
calc ffpd = calculateChunkSizeFromFFProbeData 50 <$> ffpd
