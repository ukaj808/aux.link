module AugsLink.Service.Application ( server ) where

import Data.ByteString.Lazy as Lazy
import Servant 

import AugsLink.Service.API (API)
import CommandLine (Options (staticFilePath))

handlers :: Options -> Server API
handlers opts = home
  :<|> create
  :<|> room 
  :<|> public

  where 
    create = undefined
    room id = undefined
    home = Lazy.readFile "/public/pages/home/home.html"
    public = serveDirectoryWebApp (staticFilePath opts)

server :: Options -> Application
server opts = serve (Proxy @API) (handlers opts)
