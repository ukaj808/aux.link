module AugsLink.Core.Wav
  (
    parseWavFile
  , FmtSubChunk (..)
  ) where

import Data.Aeson
import Data.Binary
import Data.Binary.Get
import GHC.Conc.IO
import GHC.Generics
import GHC.IO.Handle

import qualified Data.ByteString.Lazy as L

data RiffChunk = RiffChunk
  {
    chunkId    :: Word32
  , chunkSize  :: Word32
  , format     :: Word32
  } deriving (Show, Generic)

data FmtSubChunk = FmtSubChunk
  {
    subchunk1ID   :: Word32
  , subchunk1Size :: Word32
  , audioFormat   :: Word16
  , numChannels   :: Word16
  , sampleRate    :: Word32
  , byteRate      :: Word32
  , blockAlign    :: Word16
  , bitsPerSample :: Word16
  , cbSize        :: Word16
  , validBitsPerSample :: Word16
  , channelMask        :: Word32
  } deriving (Show, Generic)

instance ToJSON RiffChunk
instance ToJSON FmtSubChunk

getRiffChunk :: Get RiffChunk
getRiffChunk = do
  chunkId   <- getWord32be
  chunkSize <- getWord32le
  format    <- getWord32be
  return $ RiffChunk {
    chunkId   = chunkId
  , chunkSize = chunkSize
  , format    = format
  }

getFmtSubChunk :: Get FmtSubChunk
getFmtSubChunk = do
  subchunk1ID   <- getWord32be
  subchunk1Size <- getWord32le
  audioFormat   <- getWord16le
  numChannels   <- getWord16le
  sampleRate    <- getWord32le
  byteRate      <- getWord32le
  blockAlign    <- getWord16le
  bitsPerSample <- getWord16le
  cbSize        <- getWord16le --maybe?
  validBitsPerSample <- getWord16le
  channelMask        <- getWord32le
  skip 16 --skip subformat guid (128 bits)
  return $ FmtSubChunk {
    subchunk1ID   = subchunk1ID
  , subchunk1Size = subchunk1Size
  , audioFormat   = audioFormat
  , numChannels   = numChannels
  , sampleRate    = sampleRate
  , byteRate      = byteRate
  , blockAlign    = blockAlign
  , bitsPerSample = bitsPerSample
  , cbSize        = cbSize
  , validBitsPerSample  = validBitsPerSample
  , channelMask         = channelMask
  }

-- Fradgily assumes the audio data is pcm_s16le. Any other format of the audio data
-- could break the header parsing algorithm here as
-- the header format generally depends on the audio data. For example, pcm_f32le
-- will add a few more fields to the fmt sub chunk, which breaks this.
parseWavFile :: Handle -> IO (FmtSubChunk, Integer)
parseWavFile handle = do
  _           <- runGet getRiffChunk   <$> L.hGet handle 12
  fmtSubChunk <- runGet getFmtSubChunk <$> L.hGet handle 48 
  -- scrubs the offset in the handle up to the data chunk
  -- returns the ammount of bytes to pull as to not go over
  audioSizeInBytes <- findDataChunkAndSeek handle
  return (fmtSubChunk, audioSizeInBytes)
  where
    findDataChunkAndSeek :: Handle -> IO Integer
    findDataChunkAndSeek h = do
      threadDelay 100000
      subchunkId   <-                runGet getWord32be <$> L.hGet h 4
      subchunkSize <- fromIntegral . runGet getWord32le <$> L.hGet h 4
      if subchunkId == 0x64617461 -- "data"
        then do
          return subchunkSize
        else do
          hSeek h RelativeSeek subchunkSize -- scrub past the subchunk bytes
          findDataChunkAndSeek h
