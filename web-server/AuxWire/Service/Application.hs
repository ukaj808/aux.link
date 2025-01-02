{-# LANGUAGE OverloadedStrings #-}
module AuxWire.Service.Application ( server ) where

import Servant

import AuxWire.Core.API
import AuxWire.Service.API
import AuxWire.Service.Handlers.GetHome
import AuxWire.Service.Handlers.GetRoom
import AuxWire.Service.Handlers.MusicWs
import AuxWire.Service.Handlers.PostHome
import AuxWire.Service.Handlers.PostValidateUrl
import AuxWire.Service.Handlers.PutStartMusic
import AuxWire.Service.Handlers.PutUploadSong
import AuxWire.Service.Handlers.RoomWs
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
  :<|> publicHandler
  where
    publicHandler = serveDirectoryWebApp $ staticAssetsPath opts

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
