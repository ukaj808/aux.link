module AugsLink.Core.FFMpeg 
  (
    ffmpegConvertAudio
  , ffprobeAudio
  , FFProbeData (..)
  ) where

import System.Process

type FFMpegExecutableDir = FilePath
type FFProbeExecutableDir = FilePath

data FFProbeData = FFProbeData {
  sampleRate :: Int
, bitRate    :: Int
, audio      :: String
, duration   :: Double  
, numBytes   :: Int
} deriving Show

ffmpegConvertAudio :: FFMpegExecutableDir -> FilePath -> FilePath -> IO ()
ffmpegConvertAudio ffmpeg inputFile outputFile = do 
  callProcess ffmpeg ["-i", inputFile, outputFile]

ffprobeAudio :: FFProbeExecutableDir -> FilePath -> IO FFProbeData
ffprobeAudio ffprobe filePath = do
  duration <- readProcess ffprobe ["-v", "error", "-show_entries", "format=duration", "-of", "default=noprint_wrappers=1:nokey=1", "-select_streams", "a:0", filePath] ""
  bitRate <- readProcess ffprobe ["-v", "error", "-show_entries", "format=bit_rate", "-of", "default=noprint_wrappers=1:nokey=1", "-select_streams", "a:0", filePath] ""
  sampleRate <- readProcess ffprobe ["-v", "error", "-show_entries", "stream=sample_rate", "-of", "default=noprint_wrappers=1:nokey=1", "-select_streams", "a:0", filePath] ""
  audioType <- readProcess ffprobe ["-v", "error", "-show_entries", "stream=codec_name", "-of", "default=noprint_wrappers=1:nokey=1", "-select_streams", "a:0", filePath] ""
  numBytes <- readProcess ffprobe ["-v", "error", "-show_entries", "format=size", "-of", "default=noprint_wrappers=1:nokey=1", filePath] ""
  return $ FFProbeData 
    {
      sampleRate = read sampleRate
    , bitRate = read bitRate
    , audio = audioType
    , duration = read duration
    , numBytes = read numBytes
    }
