module AugsLink.Core.Wav
  (
    parseWavFile
  , FmtSubChunk (..)
  ) where

import Data.Binary
import Data.Binary.Get
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Data.Text as T
import GHC.IO.Handle
import Data.Text.Encoding
import Data.Binary.Put (runPut,putWord32be, putWord32le)

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
  } deriving (Show, Generic)

data SubChunk = SubChunk
  {
    subchunkId :: Word32
  , subchunkSize :: Word32
  } deriving (Show, Generic)

instance ToJSON RiffChunk
instance ToJSON FmtSubChunk
instance ToJSON SubChunk

getSubChunk :: Get SubChunk
getSubChunk = do
  subchunkId <- getWord32be
  subchunkSize <- getWord32le
  return $ SubChunk {
    subchunkId = subchunkId
  , subchunkSize = subchunkSize
  }


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
  return $ FmtSubChunk {
    subchunk1ID   = subchunk1ID
  , subchunk1Size = subchunk1Size
  , audioFormat   = audioFormat
  , numChannels   = numChannels
  , sampleRate    = sampleRate
  , byteRate      = byteRate
  , blockAlign    = blockAlign
  , bitsPerSample = bitsPerSample
  }

word32ToByteString :: Word32 -> L.ByteString
word32ToByteString w = runPut (putWord32be w)

parseWavFile :: Handle -> IO (FmtSubChunk, Integer)
parseWavFile handle = do 
  riffChunk <- runGet getRiffChunk <$> L.hGet handle 12
  fmtSubChunk <- runGet getFmtSubChunk <$> L.hGet handle 24
  -- Iteratively search for the 'data' chunk and set the file position at the start of the raw audio data
  audioSizeInBytes <- findDataChunkAndSeek handle
  return (fmtSubChunk, audioSizeInBytes)
  where
    findDataChunkAndSeek :: Handle -> IO Integer
    findDataChunkAndSeek h = do
      subchunkHead <- runGet getSubChunk <$> L.hGet h 8
      let bs = word32ToByteString (subchunkId subchunkHead)
      let decodedId = decodeUtf32BE $ B.toStrict bs
      let numBytesOfSubChunk = fromIntegral $ subchunkSize subchunkHead :: Integer
      print decodedId
      if decodedId == T.pack "data"
        then return numBytesOfSubChunk
        else do
          hSeek h RelativeSeek numBytesOfSubChunk -- scrub past the subchunk bytes
          findDataChunkAndSeek h
