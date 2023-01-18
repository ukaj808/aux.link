module AugsLink.Service.Application ( server ) where

import Data.Proxy (Proxy(..))
import Network.Wai (Application)
import Servant (Server, serveDirectoryWebApp)
import Servant.Server (serve)

import AugsLink.Service.API (API)


handlers :: Server API
handlers = serveDirectoryWebApp undefined

server :: Application
server = serve (Proxy @API) handlers
