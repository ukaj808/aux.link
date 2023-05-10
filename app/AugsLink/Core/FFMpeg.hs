module AugsLink.Core.FFMpeg
  (
    convertToWav
  ) where

import System.Process

convertToWav :: FilePath -> FilePath -> String -> String -> IO FilePath
convertToWav ffmpeg dir name ext = do
  let i = dir ++ "/" ++ name ++ "." ++ ext
  let o = dir ++ "/" ++ name ++ ".wav"
  callProcess ffmpeg ["-i", i, "-ac", "2", "-f", "wav", "-acodec", "pcm_f32le" , o]
  return o
