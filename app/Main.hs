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
      case env clArgs of
        Local -> 
          run port $ server clArgs roomRegistry 
        Production ->
          runTLS tlsOpts warOpts $ server clArgs roomRegistry
  where
    port = 8080
    tlsOpts = tlsSettings "cert.pem" "key.pem"
    warOpts = setPort port defaultSettings