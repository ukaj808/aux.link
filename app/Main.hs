module Main( main ) where

import Network.Wai.Handler.Warp

import AugsLink.Core.Registry
import AugsLink.Service.Application
import CommandLine
import System.IO.Temp (withTempDirectory)


main :: IO ()
main = do
  clArgs       <- getCLArgs
  withTempDirectory
    (roomsDir clArgs) 
    (roomsDirName clArgs) 
    $ \roomsPath -> do
      roomRegistry <- newRegistry roomsPath
      run 8080 $ server clArgs roomRegistry
