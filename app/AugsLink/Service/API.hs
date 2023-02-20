module AugsLink.Service.API
  ( API
  , StaticHtml(..)
  , ServerHtml
  ) where

import qualified Data.Text as T
import Servant
import Servant.API.WebSocket
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (ToMarkup, preEscapedText, preEscapedToMarkup)

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

type API =   
        Get '[HTML] StaticHtml -- Home Page
        -- Create Room Button Click on Home Page -> Create Room -> Redirect to /room/<id>
   :<|> PostSeeOther '[PlainText] (Headers '[Header "Location" String] String) 
   :<|> Capture "roomid" String :> Get '[HTML] ServerHtml
   :<|> Capture "roomid" String :> "ws" :> WebSocketPending
   :<|> "public" :> Raw
   -- Need more endpoints for music file download + delete
