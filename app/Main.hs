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
    "." 
    "rooms"
    $ \roomsPath -> do
      roomRegistry <- newRegistry roomsPath
      run port $ server clArgs roomRegistry
  where
    port = 8080