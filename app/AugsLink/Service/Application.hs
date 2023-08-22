{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Application ( server ) where

import Servant

import AugsLink.Core.API
import AugsLink.Service.API
import AugsLink.Service.Handlers.GetAudioWorker
import AugsLink.Service.Handlers.GetHome
import AugsLink.Service.Handlers.GetRoom
import AugsLink.Service.Handlers.MusicWs
import AugsLink.Service.Handlers.PostHome
import AugsLink.Service.Handlers.PostValidateUrl
import AugsLink.Service.Handlers.PutStartMusic
import AugsLink.Service.Handlers.PutUploadSong
import AugsLink.Service.Handlers.RoomWs
import CommandLine
import Network.Wai
import Network.HTTP.Types

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

middleware  :: Middleware
middleware app req sendResponse =
  app req $ \res -> do
    sendResponse $ mapResponseHeaders (defaultHeaders ++) res


defaultHeaders :: ResponseHeaders
defaultHeaders =
    [ ("Cross-Origin-Opener-Policy", "same-origin")
    , ("Cross-Origin-Embedder-Policy", "require-corp")
    ]

server :: CLArgs -> Registry IO -> Application
server opts rr = middleware $ serve (Proxy @API) (handlers opts rr)
