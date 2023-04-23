module AugsLink.Core.Wav
  (
    parseWavHeader
  , WavHeader (..)
  ) where

import Data.ByteString.Lazy
import Data.Binary
import Data.Binary.Get

data WavHeader = WavHeader
  {
    sampleRate :: Int
  , byteRate   :: Int
  } deriving (Show)

parseWavHeader :: ByteString -> WavHeader
parseWavHeader = runGet getWavHeader
  where
    getWavHeader :: Get WavHeader
    getWavHeader = do
      _ <- getByteString 4  -- RIFF
      _ <- getWord32le      -- ChunkSize
      _ <- getByteString 4  -- WAVE
      _ <- getByteString 4  -- fmt
      _ <- getWord32le      -- Subchunk1Size
      _ <- getWord16le      -- AudioFormat
      _ <- getWord16le      -- NumChannels
      sampleRate <- getWord32le
      byteRate <- getWord32le
      _ <- getWord16le      -- BlockAlign
      _ <- getWord16le      -- BitsPerSample
      _ <- getByteString 4  -- data
      _ <- getWord32le      -- Subchunk2Size
      return $ WavHeader (fromIntegral sampleRate) (fromIntegral byteRate)


