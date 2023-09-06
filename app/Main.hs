module Main( main ) where

import Network.Wai.Handler.Warp

import AugsLink.Core.Registry
import AugsLink.Service.Application
import CommandLine
import System.IO.Temp (withTempDirectory)
import Network.Wai.Handler.WarpTLS
import Network.Wai.Middleware.ForceSSL
import Network.Wai (Middleware)
import Network.Wai.Request (appearsSecure)

logSecure :: Middleware
logSecure app req sendResponse =
  app req $ \res -> do
    putStrLn $ "Request Secure: " ++ show (appearsSecure req)
    sendResponse res

main :: IO ()
main = do
  clArgs       <- getCLArgs
  withTempDirectory
    "."
    "rooms"
    $ \roomsPath -> do
      roomRegistry <- newRegistry roomsPath
      runTLS tlsOpts warOpts $
        forceSSL             $
          server clArgs roomRegistry
  where
    port = 8443
    tlsOpts = (tlsSettings "tls/warp.crt" "tls/warp.key") { onInsecure = AllowInsecure }
    warOpts = setPort port defaultSettings