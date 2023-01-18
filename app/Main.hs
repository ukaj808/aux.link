module Main ( main ) where

import Network.Wai.Handler.Warp (run)

import Augslink.Service.Application (server)

main :: IO ()
main = run 8080 server
