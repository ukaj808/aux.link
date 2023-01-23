module AugsLink.Service.Application ( server ) where

import Servant 

import AugsLink.Service.API (API, RawHtml)
import CommandLine (Options (staticFilePath))

handlers :: Options -> Server API
handlers opts = home
           :<|> create
           :<|> room 
           :<|> public

  where 
    home :: Handler RawHtml
    home = undefined

    create :: Handler RawHtml
    create = undefined

    room :: String -> Handler RawHtml
    room id = undefined

    public = serveDirectoryWebApp (staticFilePath opts)

server :: Options -> Application
server opts = serve (Proxy @API) (handlers opts)
