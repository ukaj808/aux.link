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

type PostSeeOther = Verb 'POST 303 

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

type API
   =    Get '[HTML] RawHtml -- Home Page
   :<|> PostSeeOther '[PlainText] (Headers '[Header "Location" String] String) -- Create Room Button Click on Home Page -> Create Room -> Redirect to /room/<id>
   :<|> "room" :> Capture "roomid" String :> Get '[HTML] RawHtml -- Room (where ws connection?) 
   :<|> "public" :> Raw
