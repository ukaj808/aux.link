module Main( main ) where

import Network.Wai.Handler.Warp ( run )

import AugsLink.Service.Application ( server )
import CommandLine ( getCLArgs )
import AugsLink.Service.Room (newRoomServer)

main :: IO ()
main = do
  clArgs <- getCLArgs
  roomServer <- newRoomServer
  run 8080 $ server clArgs roomServer
