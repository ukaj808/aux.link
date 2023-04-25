module AugsLink.Core.Wav
  (
    parseWavHeaderFromHandle
  , WavHeader (..)
  ) where

import Data.Binary
import Data.Binary.Get
import Data.Aeson (ToJSON)
import GHC.Generics
import qualified Data.ByteString.Lazy as L
import GHC.IO.Handle

data WavHeader = WavHeader
  {
    chunkId    :: Word32
  , chunkSize  :: Word32
  , format     :: Word32
  , subchunk1ID :: Word32
  , subchunk1Size :: Word32
  , audioFormat  :: Word16
  , numChannels  :: Word16
  , sampleRate :: Word32
  , byteRate   :: Word32
  , blockAlign :: Word16
  , bitsPerSample :: Word16
  , subchunk2ID :: Word32
  , subchunk2Size :: Word32
  } deriving (Show, Generic)

instance ToJSON WavHeader

parseWavHeader :: L.ByteString -> WavHeader
parseWavHeader = runGet getWavHeader
  where
    getWavHeader :: Get WavHeader
    getWavHeader = do
      chunkId <- getWord32be
      chunkSize <- getWord32le
      format <- getWord32be
      subchunk1ID <- getWord32be
      subchunk1Size <- getWord32le
      audioFormat <- getWord16le
      numChannels <- getWord16le
      sampleRate <- getWord32le
      byteRate <- getWord32le
      blockAlign <- getWord16le
      bitsPerSample <- getWord16le
      subchunk2ID <- getWord32be
      subchunk2Size <- getWord32le
      return $ WavHeader {
        chunkId = chunkId
      , chunkSize = chunkSize
      , format    = format
      , subchunk1ID = subchunk1ID
      , subchunk1Size = subchunk1Size
      , audioFormat = audioFormat
      , numChannels = numChannels
      , sampleRate = sampleRate
      , byteRate = byteRate
      , blockAlign = blockAlign
      , bitsPerSample = bitsPerSample
      , subchunk2ID = subchunk2ID
      , subchunk2Size = subchunk2Size
      }

parseWavHeaderFromHandle :: Handle -> IO WavHeader
parseWavHeaderFromHandle handle = parseWavHeader <$> L.hGet handle 44
