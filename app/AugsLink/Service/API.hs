{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.API
  ( 
    API
  , EnqueueSongRequest (..)
  , ScrapeUrlUploadRequest (..)
  , ScrapeUrlValidateRequest (..)
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

import AugsLink.Core.API

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
  contentType _ = "text" // "js" /: ("charset", "utf-8")

instance ToMarkup StaticHtml where
  toMarkup              = preEscapedToMarkup
  preEscapedToMarkup st = preEscapedText $ unrawHtml st

type PostSeeOther = Verb 'POST 303 

newtype ScrapeUrlUploadRequest = UrlUploadRequest
  {
    url :: Text
  } deriving (Generic, Show)
instance FromJSON ScrapeUrlUploadRequest

newtype ScrapeUrlValidateRequest = ScrapeUrlValidateRequest
  {
    u :: Text
  } deriving (Generic, Show)
instance FromJSON ScrapeUrlValidateRequest

newtype EnqueueSongRequest = EnqueueSongRequest
  {
    priority :: Int
  } deriving (Generic)
instance FromJSON EnqueueSongRequest

type API =   
        Get '[HTML] StaticHtml -- Home Page
        -- Create Room Button Click on Home Page -> Create Room -> Redirect to /room/<id>
   :<|> PostSeeOther '[PlainText] (Headers '[Header "Location" Text] NoContent) 
   :<|> Capture "roomid" Text :> Get '[HTML] 
     (
       Headers 
       '[
         Header "Cross-Origin-Opener-Policy" Text, 
         Header "Cross-Origin-Embedder-Policy" Text
        ] 
        ServerHtml
      )
   :<|> Capture "roomid" Text :> "ws" :> WebSocketPending
   :<|> Capture "roomid" Text :> "users":> Capture "userId" UserId :> "music"                       :> WebSocketPending
   :<|> Capture "roomid" Text :> "users":> Capture "userId" UserId :> "music" :> "start"            :> PutNoContent
   :<|> Capture "roomId" Text :> "users":> Capture "userId" UserId :> "music" :> "upload"           :> MultipartForm Tmp (MultipartData Tmp) :> PutNoContent

   :<|> "validate-scrape-url" :> ReqBody '[JSON] ScrapeUrlValidateRequest :> Post '[JSON] Bool
   :<|> "public" :> "audio_socket_worker_bundle.js" :> Get '[JS]
     (
       Headers 
       '[
         Header "Cross-Origin-Opener-Policy" Text, 
         Header "Cross-Origin-Embedder-Policy" Text
        ] 
        StaticJs
      )
 
   -- maybe scrape request comes through websockets because there only passing a url...
   :<|> "public" :> Raw
   -- Need more endpoints for music file download + delete
