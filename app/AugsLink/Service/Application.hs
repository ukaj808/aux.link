module AugsLink.Service.Application ( server ) where

import Servant 

import AugsLink.Service.API ( API )
import CommandLine ( Options ( staticDirPath ) )
import AugsLink.Service.Handlers.GetHome ( home )
import AugsLink.Service.Handlers.PostHome ( create )
import AugsLink.Service.Handlers.RoomWs ( join )
import AugsLink.Service.Handlers.GetRoom ( room )
import AugsLink.Service.Room (RoomServer)

handlers :: Options -> IO (RoomServer IO) -> Server API
handlers opts rserver = 
         home opts
    :<|> create rserver
    :<|> room opts
    :<|> join
    :<|> public
  where 
    public = serveDirectoryWebApp $ staticDirPath opts

server :: Options -> IO (RoomServer IO) -> Application
server opts roomServer = serve (Proxy @API) (handlers opts roomServer)
