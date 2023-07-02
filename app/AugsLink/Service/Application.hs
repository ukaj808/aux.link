module AugsLink.Service.Application ( server ) where

import Servant

import AugsLink.Core.API
import AugsLink.Service.API
import AugsLink.Service.Handlers.GetAudioWorker
import AugsLink.Service.Handlers.GetHome
import AugsLink.Service.Handlers.GetRoom
import AugsLink.Service.Handlers.MusicWs
import AugsLink.Service.Handlers.PostHome
import AugsLink.Service.Handlers.PostValidateScrapeUrl
import AugsLink.Service.Handlers.PutScrapeSong
import AugsLink.Service.Handlers.PutStartMusic
import AugsLink.Service.Handlers.PutUploadSong
import AugsLink.Service.Handlers.RoomWs
import CommandLine

handlers :: CLArgs -> Registry IO -> Server API
handlers opts rr = 
       homeHandler   opts
  :<|> createHandler rr
  :<|> roomHandler   rr
  :<|> enterHandler  rr
  :<|> connectHandler rr
  :<|> startHandler rr
  :<|> uploadHandler rr
  :<|> validateHandler
  :<|> audioWorkerHandler opts
  :<|> publicHandler
  where 
    publicHandler = serveDirectoryWebApp $ publicAssetsPath opts

server :: CLArgs -> Registry IO -> Application
server opts rr = serve (Proxy @API) (handlers opts rr) 
