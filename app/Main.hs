module Main( main ) where

import Network.Wai.Handler.Warp

import AugsLink.Core.Registry
import AugsLink.Service.Application
import CommandLine
import System.IO.Temp (withTempDirectory)
import Network.Wai.Handler.WarpTLS
import Network.Wai.Middleware.ForceSSL

main :: IO ()
main = do
  clArgs       <- getCLArgs
  withTempDirectory
    "."
    "rooms"
    $ \roomsPath -> do
      roomRegistry <- newRegistry roomsPath
      runTLS (tlsOpts (certificatePath clArgs) (privateKeyPath clArgs)) warOpts $
        forceSSL             $
          server clArgs roomRegistry
  where
    port = 8443
    tlsOpts crt privKey = (tlsSettings crt privKey) { onInsecure = AllowInsecure }
    warOpts = setPort port defaultSettings