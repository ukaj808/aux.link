module Main( main ) where

import Network.Wai.Handler.Warp ( run )

import AugsLink.Service.Application ( server )
import CommandLine ( getCLArgs )
import AugsLink.Service.Room (newRegistry)

main :: IO ()
main = do
  clArgs <- getCLArgs
  roomRegistry <- newRegistry
  run 8080 $ server clArgs roomRegistry
