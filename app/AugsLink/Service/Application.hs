module AugsLink.Service.Application ( server ) where

import Servant 

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as Lazy

import AugsLink.Service.API (API, RawHtml(..) )
import CommandLine (Options (staticFilePath))

homeHtml :: FilePath
homeHtml = "./pages/home.html"

roomHtml :: FilePath
roomHtml = "./pages/room.html"

handlers :: Options -> Server API
handlers opts = home
           :<|> create
           :<|> room 
           :<|> public

  where 
    home = do
      homeHtmlFile <- liftIO  $ Lazy.readFile homeHtml 
      return $ RawHtml homeHtmlFile

    create = undefined

    room id = undefined

    public = serveDirectoryWebApp (staticFilePath opts)

server :: Options -> Application
server opts = serve (Proxy @API) (handlers opts)
