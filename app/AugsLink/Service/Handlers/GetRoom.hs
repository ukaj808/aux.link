{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Handlers.GetRoom
  ( 
    room
  ) where

import Servant
import Control.Monad
import Control.Monad.IO.Class
import Text.Blaze.Html5
import Data.UUID

import qualified Data.Text                   as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import AugsLink.Service.API
import AugsLink.Core.API

renderUser :: RoomUser -> H.Html
renderUser user = 
  let uid = toValue $ toText $ userId user
      uname = toMarkup $ userName user
  in
  H.li ! A.id uid ! A.class_ "user-order-list__user" $ do
    H.span ! A.class_ "user-order-list__username-lbl" $ uname


renderOrderSection :: [RoomUser] -> H.Html 
renderOrderSection users = 
  H.section ! A.id "order" ! A.class_ "order" $ do
    H.ol ! A.id "user-order-list" ! A.class_ "user-order-list" $ do
      forM_ users renderUser 

renderRoomPage :: [RoomUser] -> H.Html
renderRoomPage users = H.docTypeHtml $ do
  H.head $ do
    H.title "Room"
    H.meta   ! A.charset "UTF-8"
    H.meta   ! A.name "viewport"  ! A.content "width=device-width, initial-scale=1.0"
    H.script ! A.type_ "module"   ! A.src     "/public/scripts/room.js" $ ""
    H.link   ! A.rel "stylesheet" ! A.href    "/public/styles/room.css"
    H.link   ! A.rel "icon"       ! A.type_   "image/x-icon"            ! A.href "/public/images/favicon.ico"
  H.body $ do
    H.main ! A.id "room" ! A.class_ "room" $ do
      renderOrderSection users
      H.section ! A.id "democracy" ! A.class_ "democracy" $ ""
      H.section ! A.id "drop"      ! A.class_ "drop"      $ ""
      H.section ! A.id "current"   ! A.class_ "current"   $ ""
      H.section ! A.id "queue"     ! A.class_ "queue"     $ ""

room :: Registry IO -> T.Text -> Handler ServerHtml
room registry eId = do

  let rId = case fromText eId of
              Just roomId -> roomId
              Nothing -> error "Invalid room id"

  possibleRoom <- liftIO $ getRoom registry rId

  let rm = case possibleRoom of
               Just r -> r
               Nothing -> error "Room does not exist"
  --  Maybe we need to hold lock on room somehow until result returned and confirmed. 
  roomView <- liftIO $ viewRoom rm
  
  return $ renderRoomPage roomView
