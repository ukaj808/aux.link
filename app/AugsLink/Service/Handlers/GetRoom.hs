{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Handlers.GetRoom
  (
    roomHandler
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Servant
import Text.Blaze.Html5

import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Svg11            as S
import qualified Text.Blaze.Svg11.Attributes as SVGA
import qualified Data.Aeson                  as Aeson
import qualified Data.Text                   as T

import AugsLink.Core.API
import Data.Maybe (fromMaybe)

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

renderUser :: RoomUser -> H.Html
renderUser user =
  let suid  = toValue  $ sanitizedUserId user
      uname = toMarkup $ userName user
      color = toValue $ hexColor user
      bgcolor = toValue $ "background-color: " <> hexColor user <> ";"
      classes = "square-cell tertiary-theme spaced-hz-li" :: AttributeValue
  in
  H.div ! A.id suid ! A.class_ classes ! H.dataAttribute "hex-color" color ! A.style bgcolor $ ""

renderRoomInfoSection :: H.Html
renderRoomInfoSection =
  H.section ! A.id "room-info-section" $ do
    H.span $ "123123123"

renderUserQueueSection :: UserQueueView -> H.Html
renderUserQueueSection uqv =
  H.section ! A.id "user-section" ! H.dataAttribute "og-state" jsonOv $ do
    forM_ (uqvQueue uqv) renderUser
  where
    jsonOv = textValue $ T.pack $ show $ Aeson.encode uqv

renderCurrentlyPlayingSection :: CurrentlyPlayingView -> H.Html
renderCurrentlyPlayingSection cpv =
  H.section ! A.id "music-player-section"
  ! H.dataAttribute "state" musicStateAttribute $ do
    H.div  ! A.id "cp-overlay" ! A.class_ "overlay full-flex centered z-1" $ ""
    H.canvas ! A.id "audio-visualizer" ! A.class_ "full-abs z-0" $ ""
    H.span ! A.id "cp-status" ! A.class_ "top-left-abs medium-text z-2" $ "Disconnected"
    H.button ! A.id "cp-connect-btn" ! A.class_ "top-right-abs borderless z-2" $ "Connect"
    H.span ! A.id "cp-timer"   ! A.class_ timerClasses   $ timerText
    H.span ! A.id "cp-desc"    ! A.class_ descClasses    $ descText
    H.div  ! A.id "cp-loading" ! A.class_ loadingClasses $ do
      H.div ! A.class_ "lds-facebook-md" $ do
        H.div ""
        H.div ""
        H.div ""
  where
    jsonCpv        = stringValue $ show $ Aeson.encode cpv
    timerClasses   = ("centered-abs z-2 big-text" <> if cpvState cpv /= Countdown
                                                     then (" hidden" :: AttributeValue)
                                                     else "") :: AttributeValue
    loadingClasses = ("centered-abs z-2"          <> if cpvState cpv /= Polling
                                                     then (" hidden" :: AttributeValue)
                                                     else "") :: AttributeValue
    descClasses    = ("centered-abs z-2 big-text" <> if cpvState cpv /= Streaming && cpvState cpv /= NotRunning
                                                     then (" hidden" :: AttributeValue)
                                                     else "") :: AttributeValue
    descText       = case cpvState cpv of
                        NotRunning -> "Waiting for the creator to start the music..."
                        Streaming  -> preEscapedText $ fromMaybe "" (cpvSong cpv)
                        _          -> ""
    songTitleAttribute = stringValue $ maybe "" show (cpvSong cpv)
    musicStateAttribute = stringValue $ show $ cpvState cpv
    timerDataAttribute = stringValue timerString
    timerText      = preEscapedString timerString
    timerString = if cpvState cpv == Countdown
                      then maybe "n/a" show (cpvCountdown cpv)
                      else ""

renderDropSection :: H.Html
renderDropSection =
  H.section ! A.id "drop-section" $ do
      H.label ! A.id "drop-zone" ! A.contenteditable "true" ! A.for "drop-zone-input" $ do
        H.span ! A.id "empty-drop-zone-text" $ "Click, Paste, or Drop audio files or links here!"
        H.input ! A.type_ "file" ! A.id "drop-zone-input" ! A.accept "audio/*" ! A.style "display:none" ! A.multiple "multiple"
        H.input ! A.type_ "text" ! A.id "drop-zone-paste-hack" ! A.tabindex "-1" ! A.class_ "hidden-input secondary-theme"

renderRoomPage :: RoomView -> H.Html
renderRoomPage room = H.docTypeHtml $ do
  H.head $ do
    H.title "Room"
    H.meta   ! A.charset "UTF-8"
    H.meta   ! A.name "viewport"  ! A.content "width=device-width, initial-scale=1.0"
    H.script ! A.type_ "module"   ! A.src     "/public/room.bundle.js" $ ""
    H.link   ! A.rel "stylesheet" ! A.media "screen and (min-width:0px) and (max-width:1025px)" ! A.href    "/public/room-mobile.css"
    H.link   ! A.rel "stylesheet" ! A.media "screen and (min-width:1025px)" ! A.href    "/public/room-mobile.css"
    H.link   ! A.rel "icon"       ! A.type_   "image/x-icon"            ! A.href "/public/favicon.ico"
  H.body $ do
    renderRoomInfoSection         
    renderUserQueueSection        $ uqv room
    renderCurrentlyPlayingSection $ cpv room
    renderDropSection

instance ToMarkup RoomView where
  toMarkup = renderRoomPage

roomHandler :: Registry IO
  -> RoomId
  -> Handler RoomView
roomHandler registry rId = do

  possibleRoom <- liftIO $ getRoom registry rId

  let rm = case possibleRoom of
               Just r -> r
               Nothing -> error "Room does not exist"
  --  Maybe we need to hold lock on room somehow until result returned and confirmed. 
  liftIO $ viewRoom rm
