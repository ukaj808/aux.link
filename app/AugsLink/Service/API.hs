{-# LANGUAGE OverloadedStrings #-}

module AugsLink.Service.API
  ( API
  , HTML(..)
  , RawHtml(..)
  ) where

import Data.ByteString.Lazy as Lazy
import Network.HTTP.Media ((//), (/:))
import Servant

data HTML =
  HTML

newtype RawHtml =
  RawHtml
    { unRaw :: Lazy.ByteString
    }

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

type API
   = Get '[ HTML] RawHtml :<|> Get '[ HTML] RawHtml :<|> "room" :> Capture "roomid" String :> Get '[ HTML] RawHtml :<|> "public" :> Raw
