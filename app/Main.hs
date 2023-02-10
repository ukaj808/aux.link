module Main( main ) where

import Network.Wai.Handler.Warp ( run )

import AugsLink.Service.Application ( server )
import CommandLine ( getOptions )
import AugsLink.Service.Room (newRoomServer)

main :: IO ()
main = do
  options <- getOptions
  roomServer <- newRoomServer
  print options
  run 8080 $ server options roomServer
