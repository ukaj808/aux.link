module AugsLink.Service.Application ( server ) where

import Servant

import AugsLink.Core.API
import AugsLink.Service.API
import AugsLink.Service.Handlers.DeleteSong
import AugsLink.Service.Handlers.GetHome
import AugsLink.Service.Handlers.GetRoom
import AugsLink.Service.Handlers.PostHome
import AugsLink.Service.Handlers.PutEnqueueSong
import AugsLink.Service.Handlers.PutMoveSong
import AugsLink.Service.Handlers.PutScrapeSong
import AugsLink.Service.Handlers.PutUploadSong
import AugsLink.Service.Handlers.RoomWs
import CommandLine

handlers :: CLArgs -> Registry IO -> Server API
handlers opts rr = 
       home   opts
  :<|> create rr
  :<|> room   rr
  :<|> enter  rr
  :<|> enqueue rr
  :<|> remove rr
  :<|> move rr
  :<|> upload rr
  :<|> scrape
  :<|> public
  where 
    public = serveDirectoryWebApp $ staticDirPath opts

server :: CLArgs -> Registry IO -> Application
server opts rr = serve (Proxy @API) (handlers opts rr) 
