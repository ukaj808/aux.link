module Main ( main ) where

import Network.Wai.Handler.Warp (run)

import AugsLink.Service.Application (server)
import CommandLine (getOptions)

main :: IO ()
main = do
  options <- getOptions
  print options
  run 8080 server
