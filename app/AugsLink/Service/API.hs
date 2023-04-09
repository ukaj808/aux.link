module AugsLink.Service.API
  ( 
    API
  , EnqueueSongRequest (..)
  , ScrapeSongRequest (..)
  , ServerHtml
  , StaticHtml (..)
  ) where

import Data.Aeson
import Data.Text
import GHC.Generics
import Servant
import Servant.API.WebSocket
import Servant.HTML.Blaze
import Servant.Multipart
import Text.Blaze.Html5
import AugsLink.Core.API

type ServerHtml    = Html

newtype StaticHtml = StaticHtml
  { 
    unRaw :: Text 
  }

instance ToMarkup StaticHtml where
  toMarkup              = preEscapedToMarkup
  preEscapedToMarkup st = preEscapedText $ unRaw st

type PostSeeOther = Verb 'POST 303 

newtype ScrapeSongRequest = ScrapeSongRequest
  {
    url :: Text
  } deriving (Generic, Show)
instance FromJSON ScrapeSongRequest

newtype EnqueueSongRequest = EnqueueSongRequest
  {
    priority :: Int
  } deriving (Generic)
instance FromJSON EnqueueSongRequest

type API =   
        Get '[HTML] StaticHtml -- Home Page
        -- Create Room Button Click on Home Page -> Create Room -> Redirect to /room/<id>
   :<|> PostSeeOther '[PlainText] (Headers '[Header "Location" Text] Text) 
   :<|> Capture "roomid" Text :> Get '[HTML] ServerHtml
   :<|> Capture "roomid" Text :> "ws" :> WebSocketPending

   :<|> Capture "roomId" Text :> Capture "userId" UserId 
     :> "songs" :> ReqBody '[JSON] EnqueueSongRequest :> Put '[PlainText] Text

   :<|> Capture "roomId" Text :> Capture "userId" UserId 
     :> "songs" :> Capture "songId" Text :> Capture "priority" Int :> Put '[PlainText] Text

   :<|> Capture "roomId" Text :> Capture "userId" UserId
     :> "songs" :> Capture "songId" Text :> "upload" 
       :> MultipartForm Mem (MultipartData Mem) :> Put '[PlainText] Text

   :<|> Capture "roomId" Text :> Capture "userId" UserId
     :> "songs" :> Capture "songId" Text :> Delete '[PlainText] Text

   :<|> Capture "roomid" Text :> Capture "userId" UserId :> "music" :> "listen"           :> WebSocketPending
   :<|> Capture "roomid" Text :> Capture "userId" UserId :> "music" :> "stop-listening"   :> Put '[PlainText] Text
   :<|> Capture "roomid" Text :> Capture "userId" UserId :> "music" :> "start"            :> Put '[PlainText] Text

   -- maybe scrape request comes through websockets because there only passing a url...
   :<|> "public" :> Raw
   -- Need more endpoints for music file download + delete
