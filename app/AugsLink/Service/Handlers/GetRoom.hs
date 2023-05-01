{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Handlers.GetRoom
  ( 
    roomHandler
  ) where

import Servant
import Control.Monad
import Control.Monad.IO.Class
import Text.Blaze.Html5
import Data.Text

import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import AugsLink.Service.API
import AugsLink.Core.API

renderUser :: RoomUser -> H.Html
renderUser user = 
  let uid = toValue $ userId user
      uname = toMarkup $ userName user
  in
  H.li ! A.id uid ! A.class_ "full-flex section centered tertiary-theme" $ do
    H.span ! A.class_ "user-order-list__username-lbl" $ uname


renderOrderSection :: [RoomUser] -> H.Html 
renderOrderSection users = 
  H.section ! A.id "order" ! A.class_ "full-flex section centered" $ do
    H.ol ! A.id "user-order-list" ! A.class_ "full-flex section centered secondary-theme" $ do
      forM_ users renderUser 

renderCurrentlyPlayingSection :: H.Html
renderCurrentlyPlayingSection = 
  H.section ! A.id "currently-playing"   ! A.class_ "full-flex section centered" $ "Click to connect to music"

renderDropSection :: H.Html
renderDropSection = 
  H.section ! A.id "drop"      ! A.class_ "full-flex section centered"  $ do
    H.div ! A.id "drop-zone" ! A.class_ "full-flex section centered secondary-theme" $ ""

renderRoomPage :: [RoomUser] -> H.Html
renderRoomPage users = H.docTypeHtml $ do
  H.head $ do
    H.title "Room"
    H.meta   ! A.charset "UTF-8"
    H.meta   ! A.name "viewport"  ! A.content "width=device-width, initial-scale=1.0"
    H.script ! A.type_ "module"   ! A.src     "/public/room_bundle.js" $ ""
    H.link   ! A.rel "stylesheet" ! A.href    "/public/room.css"
    H.link   ! A.rel "icon"       ! A.type_   "image/x-icon"            ! A.href "/public/favicon.ico"
  H.body $ do
    H.main ! A.id "room" ! A.class_ "full-flex column" $ do
      renderOrderSection users
      renderCurrentlyPlayingSection
      renderDropSection

roomHandler :: Registry IO 
  -> RoomId 
  -> Handler (
       Headers 
       '[
         Header "Cross-Origin-Opener-Policy" Text, 
         Header "Cross-Origin-Embedder-Policy" Text
        ] 
        ServerHtml
      )
roomHandler registry rId = do

  possibleRoom <- liftIO $ getRoom registry rId

  let rm = case possibleRoom of
               Just r -> r
               Nothing -> error "Room does not exist"
  --  Maybe we need to hold lock on room somehow until result returned and confirmed. 
  roomView <- liftIO $ viewRoom rm
  
  return 
    ( 
      addHeader "same-origin"       $
      addHeader "require-corp"      $
      renderRoomPage roomView
    )
