{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Handlers.GetRoom
  ( room
  ) where

import Servant
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import AugsLink.Service.API
import Text.Blaze.Html5 ((!), toValue, toMarkup)
import AugsLink.Core.API
import Data.UUID (toText, fromString)
import Data.List (sort)

renderUser :: User -> H.Html
renderUser u = 
  let uid = toValue $ toText $ userId u
      uname = toMarkup $ userName u
      ord = toMarkup $ spotInLine u
  in
  H.li ! A.id uid ! A.class_ "user-order-list__user" $ do
    H.span ! A.class_ "user-order-list__order-lbl" $ ord
    H.span ! A.class_ "user-order-list__username-lbl" $ uname


renderOrderSection :: [User] -> H.Html 
renderOrderSection users = 
  H.section ! A.id "order" ! A.class_ "order" $ do
  H.ol ! A.id "user-order-list" ! A.class_ "user-order-list" $ do
    forM_ (sort users) renderUser 

renderRoomPage :: [User] -> H.Html
renderRoomPage users = H.docTypeHtml $ do
  H.head $ do
    H.title "Room"
    H.meta ! A.charset "UTF-8"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
    H.script ! A.type_ "module" ! A.src "/public/scripts/room.js" $ ""
    H.link ! A.rel "stylesheet" ! A.href "/public/styles/room.css"
    H.link ! A.rel "icon" ! A.type_ "image/x-icon" ! A.href "/public/images/favicon.ico"
  H.body $ do
    H.main ! A.id "room" ! A.class_ "room" $ do
      renderOrderSection users
      H.section ! A.id "democracy" ! A.class_ "democracy" $ ""
      H.section ! A.id "drop" ! A.class_ "drop" $ ""
      H.section ! A.id "current" ! A.class_ "current" $ ""
      H.section ! A.id "queue" ! A.class_ "queue" $ ""

room :: Registry IO -> String -> Handler ServerHtml
room registry eId = do

  let rId = case fromString eId of
              Just roomId -> roomId
              Nothing -> error "Invalid room id"

  possibleRoom <- liftIO $ getRoom registry rId

  let rm = case possibleRoom of
               Just r -> r
               Nothing -> error "Room does not exist"
  
  users <- liftIO $ presentInRoom rm

  return $ renderRoomPage users
