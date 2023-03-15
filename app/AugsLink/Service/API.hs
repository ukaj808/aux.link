module AugsLink.Service.API
  ( API
  , ScrapeSongRequest (..)
  , ServerHtml
  , StaticHtml(..)
  ) where

import qualified Data.Text as T
import Servant
import Servant.API.WebSocket
import Servant.HTML.Blaze
import Servant.Multipart
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (ToMarkup, preEscapedText, preEscapedToMarkup)
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

type    ServerHtml = H.Html

newtype StaticHtml =
  StaticHtml
    { 
       unRaw :: T.Text 
    }

instance ToMarkup StaticHtml where
  toMarkup = preEscapedToMarkup
  preEscapedToMarkup st = preEscapedText $ unRaw st

type PostSeeOther = Verb 'POST 303 

newtype ScrapeSongRequest = ScrapeSongRequest
  {
    url :: T.Text
  } deriving (Generic, Show)

instance FromJSON ScrapeSongRequest

type API =   
        Get '[HTML] StaticHtml -- Home Page
        -- Create Room Button Click on Home Page -> Create Room -> Redirect to /room/<id>
   :<|> PostSeeOther '[PlainText] (Headers '[Header "Location" T.Text] T.Text) 
   :<|> Capture "roomid" T.Text :> Get '[HTML] ServerHtml
   :<|> Capture "roomid" T.Text :> "ws" :> WebSocketPending
   :<|> Capture "roomId" T.Text :> "songs" :> Capture "songId" T.Text :> "upload" :> MultipartForm Mem (MultipartData Mem) :> Put '[PlainText] T.Text
   :<|> Capture "roomId" T.Text :> "songs" :> "scrape" :> ReqBody '[JSON] ScrapeSongRequest     :> Put '[PlainText] T.Text
   -- maybe scrape request comes through websockets because there only passing a url...
   :<|> "public" :> Raw
   -- Need more endpoints for music file download + delete
