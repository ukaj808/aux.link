{-# LANGUAGE OverloadedStrings #-}
module AuxWire.Service.API
  ( 
    API
  , EnqueueSongRequest (..)
  , ValidateUrlRequest (..)
  , ServerHtml
  , StaticHtml (..)
  , StaticJs (..)
  ) where

import Data.Aeson
import Data.Text
import GHC.Generics
import Servant
import Servant.API.WebSocket
import Servant.HTML.Blaze
import Servant.Multipart
import Text.Blaze.Html5
import Network.HTTP.Media

import qualified Data.ByteString.Lazy as Lazy

import AuxWire.Core.API

type ServerHtml    = Html

data JS = JS

newtype StaticHtml = StaticHtml
  { 
    unrawHtml :: Text 
  }
newtype StaticJs  = StaticJs
  { 
    unrawJs :: Lazy.ByteString 
  }

instance MimeRender JS StaticJs where
  mimeRender _ = unrawJs

instance Servant.Accept JS where
  contentType _ = "text" // "javascript" /: ("charset", "utf-8")

instance ToMarkup StaticHtml where
  toMarkup              = preEscapedToMarkup
  preEscapedToMarkup st = preEscapedText $ unrawHtml st

type PostSeeOther = Verb 'POST 303 

newtype ValidateUrlRequest = ValidateUrlRequest
  {
    url :: Text
  } deriving (Generic, Show)
instance FromJSON ValidateUrlRequest

newtype EnqueueSongRequest = EnqueueSongRequest
  {
    priority :: Int
  } deriving (Generic)
instance FromJSON EnqueueSongRequest

type API =   
        Get '[HTML] StaticHtml -- Home Page
        -- Create Room Button Click on Home Page -> Create Room -> Redirect to /room/<id>
   :<|> PostSeeOther '[PlainText] (Headers '[Header "Location" Text] NoContent) 
   :<|> Capture "roomid" Text :> Get '[JSON, HTML] RoomView
   :<|> Capture "roomid" Text :> "ws"     :> WebSocketPending
   :<|> Capture "roomid" Text :> "music"  :> WebSocketPending
   :<|> Capture "roomid" Text :> Header "X-User-Id" Text :> "start"  :> PutNoContent
   :<|> Capture "roomId" Text :> Header "X-User-Id" Text :> "upload" :> MultipartForm Tmp (MultipartData Tmp) :> PutNoContent

   :<|> "validate-url" :> ReqBody '[JSON] ValidateUrlRequest :> Post '[PlainText] Text
 
   -- maybe scrape request comes through websockets because there only passing a url...
   :<|> "public" :> Raw
   -- Need more endpoints for music file download + delete
