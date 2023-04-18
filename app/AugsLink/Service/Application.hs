module AugsLink.Service.Application ( server ) where

import Servant

import AugsLink.Core.API
import AugsLink.Service.API
import AugsLink.Service.Handlers.DeleteSong
import AugsLink.Service.Handlers.GetHome
import AugsLink.Service.Handlers.GetRoom
import AugsLink.Service.Handlers.ListenWs
import AugsLink.Service.Handlers.PostHome
import AugsLink.Service.Handlers.PutEnqueueSong
import AugsLink.Service.Handlers.PutMoveSong
import AugsLink.Service.Handlers.PutStartMusic
import AugsLink.Service.Handlers.PutStopListen
import AugsLink.Service.Handlers.PutUploadSong
import AugsLink.Service.Handlers.RoomWs
import CommandLine

handlers :: CLArgs -> Registry IO -> Server API
handlers opts rr = 
       homeHandler   opts
  :<|> createHandler rr
  :<|> roomHandler   rr
  :<|> enterHandler  rr
  :<|> enqueueHandler rr
  :<|> moveHandler rr
  :<|> uploadHandler rr
  :<|> removeHandler rr
  :<|> listenHandler rr
  :<|> stopListenHandler rr
  :<|> startHandler rr
  :<|> publicHandler
  where 
    publicHandler = serveDirectoryWebApp $ staticDirPath opts

server :: CLArgs -> Registry IO -> Application
server opts rr = serve (Proxy @API) (handlers opts rr) 
