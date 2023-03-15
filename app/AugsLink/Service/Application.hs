module AugsLink.Service.Application ( server ) where

import Servant

import AugsLink.Core.API
import AugsLink.Service.API
import CommandLine
import AugsLink.Service.Handlers.GetHome
import AugsLink.Service.Handlers.PostHome
import AugsLink.Service.Handlers.RoomWs
import AugsLink.Service.Handlers.GetRoom
import AugsLink.Service.Handlers.PostScrapeSong
import AugsLink.Service.Handlers.PostUploadSong

handlers :: CLArgs -> Registry IO -> Server API
handlers opts rr = 
         home opts
    :<|> create rr
    :<|> room rr
    :<|> enter rr
    :<|> upload rr
    :<|> scrape
    :<|> public
  where 
    public = serveDirectoryWebApp $ staticDirPath opts

server :: CLArgs -> Registry IO -> Application
server opts rr = serve (Proxy @API) (handlers opts rr) 
