module Main( main ) where

import Network.Wai.Handler.Warp

import AugsLink.Core.Registry
import AugsLink.Service.Application
import CommandLine

main :: IO ()
main = do
  clArgs       <- getCLArgs
  roomRegistry <- newRegistry
  run 8080 $ server clArgs roomRegistry
