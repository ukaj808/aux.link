module AugsLink.Service.Application ( server ) where

import Servant 

import AugsLink.Service.API ( API )
import CommandLine ( Options ( staticDirPath ) )
import AugsLink.Service.Handlers.GetHome ( home )
import AugsLink.Service.Handlers.PostHome ( create )
import AugsLink.Service.Handlers.RoomWs ( join )
import AugsLink.Service.Handlers.GetRoom ( room )

handlers :: Options -> Server API
handlers opts = 
         home opts
    :<|> create
    :<|> room opts
    :<|> join
    :<|> public
  where 
    public = serveDirectoryWebApp $ staticDirPath opts

server :: Options -> Application
server opts = serve (Proxy @API) (handlers opts)
