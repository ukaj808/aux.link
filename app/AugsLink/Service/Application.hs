module AugsLink.Service.Application ( server ) where

import Servant 

import AugsLink.Service.API ( API )
import CommandLine ( CLArgs ( staticDirPath ) )
import AugsLink.Service.Handlers.GetHome ( home )
import AugsLink.Service.Handlers.PostHome ( create )
import AugsLink.Service.Handlers.RoomWs ( join )
import AugsLink.Service.Handlers.GetRoom ( room )
import AugsLink.Service.Room ( RoomControl )

handlers :: CLArgs -> RoomControl IO -> Server API
handlers opts rc = 
         home opts
    :<|> create rc
    :<|> room opts
    :<|> join rc
    :<|> public
  where 
    public = serveDirectoryWebApp $ staticDirPath opts

server :: CLArgs -> RoomControl IO -> Application
server opts rc = serve (Proxy @API) (handlers opts rc) 
