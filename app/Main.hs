module Main( main ) where

import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS

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
      runTLS tlsOpts warOpts $ server clArgs roomRegistry
  where
    port = 8080
    tlsOpts = tlsSettings "tls/warp.crt" "tls/warp.key"
    warOpts = setPort port defaultSettings