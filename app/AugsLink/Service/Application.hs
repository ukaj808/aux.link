module AugsLink.Service.Application ( server ) where

import Data.Proxy (Proxy(..))
import Network.Wai (Application)
import Servant (Server, serveDirectoryWebApp)
import Servant.Server (serve)

import AugsLink.Service.API (API)
import CommandLine (Options (staticFilePath))


handlers :: Options -> Server API
handlers opts = serveDirectoryWebApp (staticFilePath opts)

server :: Options -> Application
server opts = serve (Proxy @API) (handlers opts)
