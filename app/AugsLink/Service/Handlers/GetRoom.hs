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

import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as SVGA

import AugsLink.Service.API
import AugsLink.Core.API

musicIconSvg :: S.Svg
musicIconSvg = S.docTypeSvg ! SVGA.class_ "centered-icon" ! SVGA.version "1.1" ! SVGA.id_ "Capa_1" ! SVGA.viewbox "0 0 98.121 98.121" ! SVGA.xmlSpace "preserve" $
    S.g $ do
        S.g $ do
            S.path ! SVGA.d "M23.819,14.874h3.273v21.797c0,1.646,1.331,2.973,2.968,2.973h8.102c1.637,0,2.971-1.327,2.971-2.973V14.874h3.276\n\
                        \c0.751,0,1.435-0.438,1.752-1.119c0.313-0.686,0.202-1.485-0.283-2.061L37.109,1.385C36.362,0.504,35.267,0,34.111,0\n\
                        \c-1.151,0-2.247,0.504-2.998,1.385l-8.766,10.309c-0.488,0.576-0.598,1.375-0.28,2.061C22.38,14.436,23.067,14.874,23.819,14.874z"
            S.path ! SVGA.d "M79.199,18.616h-35.5V29.18h32.037v40.025H22.271V29.181h1.828V18.616H18.85c-3.891,0-7.037,2.948-7.037,6.837v47.091\n\
                        \c0,3.905,3.146,7.218,7.037,7.218h24.934v7.794h-9.259c-3.54,0-6.593,2.877-6.593,6.41v4.155h42.246v-4.155\n\
                        \c0-3.533-2.717-6.41-6.249-6.41h-9.591v-7.794h24.859c3.896,0,7.111-3.312,7.111-7.218v-47.09\n\
                        \C86.31,21.565,83.094,18.616,79.199,18.616z"
            S.path ! SVGA.d "M56.623,51.158c-3.314,0-6.006,2.048-6.006,4.574c0,2.522,2.69,4.57,6.006,4.57s6.008-2.048,6.008-4.57\n\
                        \c0-0.099,0-11.625,0-18.042c0-0.97-0.469-1.882-1.258-2.447c-0.787-0.564-1.802-0.714-2.723-0.403l-13.208,4.472\n\
                        \c-1.687,0.572-2.822,2.151-2.834,3.934l-0.043,11.58c-0.366-0.053-0.739-0.084-1.124-0.084c-3.315,0-6.006,2.049-6.006,4.572\n\
                        \s2.69,4.574,6.006,4.574s6.006-2.051,6.006-4.574V48.023c0-0.741,0.473-1.4,1.175-1.64l7.69-2.605\n\
                        \c0.332-0.108,0.695-0.055,0.979,0.147c0.287,0.207,0.455,0.533,0.455,0.884v6.43\n\
                        \C57.379,51.188,57.008,51.158,56.623,51.158z"

playIconSvg :: S.Svg
playIconSvg = S.docTypeSvg ! SVGA.class_ "centered-icon" ! SVGA.version "1.1" ! SVGA.id_ "Capa_1" ! SVGA.viewbox "0 0 60 60" ! SVGA.xmlSpace "preserve" $
    S.g $ do
        S.path ! SVGA.d "M45.563,29.174l-22-15c-0.307-0.208-0.703-0.231-1.031-0.058C22.205,14.289,22,14.629,22,15v30\n\
                        \c0,0.371,0.205,0.711,0.533,0.884C22.679,45.962,22.84,46,23,46c0.197,0,0.394-0.059,0.563-0.174l22-15\n\
                        \C45.836,30.64,46,30.331,46,30S45.836,29.36,45.563,29.174z M24,43.107V16.893L43.225,30L24,43.107z"
        S.path ! SVGA.d "M30,0C13.458,0,0,13.458,0,30s13.458,30,30,30s30-13.458,30-30S46.542,0,30,0z M30,58C14.561,58,2,45.439,2,30\n\
                        \S14.561,2,30,2s28,12.561,28,28S45.439,58,30,58z"                        

renderUser :: RoomUser -> H.Html
renderUser user = 
  let uid = toValue $ userId user
      uname = toMarkup $ userName user
  in
  H.div ! A.id uid ! A.class_ "user-carousel-cell" $ ""


renderOrderSection :: [RoomUser] -> H.Html 
renderOrderSection users = 
  H.section ! A.id "order" ! A.class_ "default-margin" $ do
    H.div ! A.class_"user-carousel" $ do
      forM_ users renderUser 

renderCurrentlyPlayingSection :: H.Html
renderCurrentlyPlayingSection = 
  H.section ! A.id "currently-playing"   ! A.class_ "full-flex centered default-margin secondary-theme overlay-sect overlay" $ do
    H.div ! A.class_ "overlay full-flex centered" $ do
      playIconSvg

renderDropSection :: H.Html
renderDropSection = 
  H.section ! A.id "drop"      ! A.class_ "full-flex centered default-margin secondary-theme"  $ do
    H.label ! A.for "drop-zone" ! A.class_ "full-flex centered column draggable" $ do
      musicIconSvg
      H.h4 "Upload an audio file or paste a link to get your queue ready in time for your turn!"
      H.input ! A.type_ "file" ! A.id "drop-zone" ! A.accept "audio/*" ! A.style "display:none"

renderRoomPage :: [RoomUser] -> H.Html
renderRoomPage users = H.docTypeHtml $ do
  H.head $ do
    H.title "Room"
    H.meta   ! A.charset "UTF-8"
    H.meta   ! A.name "viewport"  ! A.content "width=device-width, initial-scale=1.0"
    H.script ! A.type_ "module"   ! A.src     "/public/room_bundle.js" $ ""
    H.link   ! A.rel "stylesheet" ! A.href    "https://unpkg.com/flickity@2/dist/flickity.min.css"
    H.link   ! A.rel "stylesheet" ! A.href    "/public/room.css"
    H.link   ! A.rel "icon"       ! A.type_   "image/x-icon"            ! A.href "/public/favicon.ico"
  H.body $ do
    H.main ! A.id "room" ! A.class_ "full-flex column" $ do
      renderOrderSection users
      renderCurrentlyPlayingSection
      renderDropSection
      H.script ! A.src "https://unpkg.com/flickity@2/dist/flickity.pkgd.min.js" $ ""



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
      addHeader "credentialless"      $
      renderRoomPage roomView
    )
