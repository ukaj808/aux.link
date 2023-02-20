module AugsLink.Service.API
  ( API
  ) where

import Data.ByteString.Lazy as Lazy
import Servant
import Servant.API.WebSocket
import Servant.HTML.Lucid

type PostSeeOther = Verb 'POST 303 

type API =   
        Get '[HTML] Lazy.ByteString -- Home Page
        -- Create Room Button Click on Home Page -> Create Room -> Redirect to /room/<id>
   :<|> PostSeeOther '[PlainText] (Headers '[Header "Location" String] String) 
   :<|> Capture "roomid" String :> Get '[HTML] Lazy.ByteString
   :<|> Capture "roomid" String :> "ws" :> WebSocketPending
   :<|> "public" :> Raw
   -- Need more endpoints for music file download + delete
