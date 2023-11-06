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

renderUser :: RoomUser -> Int -> Int -> H.Html
renderUser user left zIndex =
  H.div ! A.id (toValue  $ sanitizedUserId user) ! A.class_ "user" ! 
    H.dataAttribute "hex-color" (toValue $ hexColor user) ! 
    H.dataAttribute "mobile-z-index" (toValue $ show zIndex) !
    H.dataAttribute "mobile-left" (toValue $ show left ++ "px") $ ""

renderRoomInfoSection :: RoomId -> H.Html
renderRoomInfoSection rId =
  H.section ! A.id "room-info-section" $ do
    H.span $ toMarkup $ "Room ID: " ++   take 5 (T.unpack rId) ++ " ..."

renderUserQueueSection :: UserQueueView -> H.Html
renderUserQueueSection uqv =
  H.section ! A.id "user-section" ! H.dataAttribute "og-state" jsonOv $ do
    foldM_ (\(left, zIndex) user -> do
      renderUser user left zIndex
      return (left + 20, zIndex - 1)
      ) (0, zIndexStart) $ uqvQueue uqv
  where
    zIndexStart = subtract 1 $ length (uqvQueue uqv)
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
                        NotRunning -> ""
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
        H.span ! A.id "drop-zone-title" $ "Queue"
        H.input ! A.type_ "file" ! A.id "drop-zone-input" ! A.accept "audio/*" ! A.style "display:none" ! A.multiple "multiple"
        H.input ! A.type_ "text" ! A.id "drop-zone-paste-hack" ! A.tabindex "-1" ! A.class_ "hidden-input secondary-theme"

renderRoomPage :: RoomView -> H.Html
renderRoomPage room = H.docTypeHtml $ do
  H.head $ do
    H.title "Room"
    H.meta   ! A.charset "UTF-8"
    H.meta   ! A.name "viewport"  ! A.content "width=device-width, initial-scale=1.0"
    H.style  ! A.id "room-mobile-stylesheet" ! A.media "screen and (min-width:0px) and (max-width:1025px)" $ roomMobileCss
    H.style ! A.id "room-desktop-stylesheet" ! A.media "screen and (min-width:1025px)" $ roomDesktopCss
    H.link   ! A.rel "icon"       ! A.type_   "image/x-icon"            ! A.href "/public/favicon.ico"
  H.body $ do
    renderRoomInfoSection         $ rId room
    renderUserQueueSection        $ uqv room
    renderCurrentlyPlayingSection $ cpv room
    renderDropSection
    H.script ! A.src     "/public/room.bundle.js" $ ""

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

roomMobileCss :: H.Html
roomMobileCss = 
  ".hidden-edit,.hidden-input{caret-color:transparent}.overlay,.z-1{z-index:1}.lds-facebook-md div,.lds-facebook-sm div{background:#fff;animation:1.2s cubic-bezier(0,.5,.5,1) infinite lds-facebook}.big-text,.medium-text,.song-list-item,.warn-btn,h4{color:var(--primary-fnt-color)}:root{--primary-bg-color:#202125;--primary-fnt-color:white;--primary-btn-bg-color:#89b3f7;--secondary-bg-color:#303135;--tertiary-bg-color:#404046;--warning-color:#ff0000}body,html{margin:0}html{display:flex;height:100vh}body{flex:1;display:grid;grid-template-columns:1fr 1.5fr;grid-template-rows:100px 5fr 1fr;grid-template-areas:\"risec      usec\" \"dropsec    dropsec\" \"musec      musec\"}#room-info-section{grid-area:risec;display:flex;justify-content:center;align-items:center;text-align:center;font-size:1.5rem;line-height:2rem;font-weight:700}#user-section{grid-area:usec;position:relative;overflow:hidden}#drop-section{grid-area:dropsec;display:flex;padding:.5rem}#drop-zone{flex:1;display:flex;flex-direction:column;justify-content:center;align-items:center;border:1px solid #f3f4f6;border-radius:.25rem}#music-player-section{grid-area:musec;position:relative}#drop-zone-title{align-self:flex-start;padding:.5rem}.user{position:absolute;top:0;left:0;bottom:0;margin:auto;height:40px;width:40px;border-radius:50%}.pos-rel{position:relative}.hidden{display:none}.draggable{border:2px dashed #fff}.column{flex-direction:column}.row{flex-direction:row}.full-abs{position:absolute;width:100%;height:100%}.centered-abs{position:absolute;top:50%;left:50%;transform:translate(-50%,-50%)}.overlay,.top-left-abs{top:0;left:0;position:absolute}.top-right-abs{position:absolute;top:0;right:0}.flex-1{flex:1}.default-margin{margin:5px}.centered{justify-content:center;align-items:center;text-align:center}.list-contain{display:flex;flex-grow:1;flex-direction:column;justify-content:flex-start}.horiz-list{display:flex;padding-left:0;margin:0;list-style:none}.x-scroll{overflow-x:auto}.square-cell{width:50%;max-width:250px;flex:0 0 auto}.overlay{width:100%;height:100%;background:radial-gradient(ellipse at center,rgba(0,0,0,.5) 0,rgba(0,0,0,0) 100%);opacity:1;transition:opacity .2s}.invisible{opacity:0}.handle{cursor:grab}.hidden-input{border:none;height:0;width:0}.secondary-theme,.user-carousel{background-color:var(--secondary-bg-color)}.tertiary-theme{background-color:var(--tertiary-bg-color)}.primary-icon-stroke{stroke:var(--primary-fnt-color);stroke-width:2;stroke-linecap:round;stroke-linejoin:round}.centered-icon{height:25%;fill:var(--primary-fnt-color)}.warn-btn{background-color:red}.warn-btn:hover{cursor:pointer}.audio-canvas-sm,.full-width{width:100%}.z-0{z-index:0}.z-2{z-index:2}.spaced-hz-li{margin-left:10px}.song-queue-list{list-style-type:none;padding:0;margin:0;display:flex;flex-direction:column;width:100%;counter-reset:item;max-height:250px;overflow-y:auto}.song-list-item{background-color:var(--tertiary-bg-color);padding:10px;margin-bottom:5px;display:flex;align-items:center;justify-content:space-between}.song-title::before{content:counter(item);counter-increment:item;font-weight:700;color:#fff;padding:5px;margin-right:10px}.song-title{display:flex;align-items:center;max-width:225px}.text-ellipsis{white-space:nowrap;overflow:hidden;text-overflow:ellipsis;display:inline-block}.borderless{border:none}.small-btn{width:50px}.warning-hover:hover{fill:var(--warning-color)}.user-carousel-cell{width:200px;height:200px;margin-right:10px;background:var(--tertiary-bg-color);counter-increment:gallery-cell}.lds-facebook-sm{display:inline-block;width:40px;height:40px;position:relative}.lds-facebook-sm div{position:absolute;width:8px;height:32px}.lds-facebook-sm div:first-child{left:4px;animation-delay:-.24s}.lds-facebook-sm div:nth-child(2){left:16px;animation-delay:-.12s}.lds-facebook-sm div:nth-child(3){left:28px;animation-delay:0s}.lds-facebook-md{display:inline-block;position:relative;width:80px;height:80px}.lds-facebook-md div{display:inline-block;position:absolute;left:8px;width:16px}.lds-facebook-md div:first-child{left:8px;animation-delay:-.24s}.lds-facebook-md div:nth-child(2){left:32px;animation-delay:-.12s}.lds-facebook-md div:nth-child(3){left:56px;animation-delay:0}.medium-text{font-size:medium}.big-text{font-size:xx-large}@keyframes lds-facebook{0%{top:4px;height:24px}100%,50%{top:12px;height:12px}}#user-queue>li:first-of-type{border:2px solid gold;animation:2s infinite pulsate}@keyframes pulsate{0%,100%{border-color:gold}50%{border-color:transparent}}"

roomDesktopCss :: H.Html
roomDesktopCss =
  ".hidden-edit,.hidden-input{caret-color:transparent}.overlay,.z-1{z-index:1}.lds-facebook-md div,.lds-facebook-sm div{background:#fff;animation:1.2s cubic-bezier(0,.5,.5,1) infinite lds-facebook}.big-text,.medium-text,.song-list-item,.warn-btn,h4{color:var(--primary-fnt-color)}:root{--primary-bg-color:#202125;--primary-fnt-color:white;--primary-btn-bg-color:#89b3f7;--secondary-bg-color:#303135;--tertiary-bg-color:#404046;--warning-color:hsl(0, 100%, 50%)}body,html{margin:0}html{display:flex;height:100vh}body{flex:1;display:grid;grid-template-columns:1fr 1.5fr;grid-template-rows:125px 3fr .5fr;grid-template-areas:\"risec      musec\" \"usec    dropsec\" \"usec    dropsec\"}#room-info-section{grid-area:risec;display:flex;justify-content:center;align-items:center;background-color:#faebd7}#user-section{grid-area:usec;position:relative}.user{width:96%;height:10%;position:absolute;left:0;right:0;top:10px;margin:auto}#drop-zone,.full-abs{width:100%;height:100%}#83ed1135-2f1f-4b4c-bfa2-03aa6fe31700{background-color:#00dcff;z-index:0;left:0}#drop-section{grid-area:dropsec;background-color:#2f4f4f}#drop-zone{display:flex;justify-content:center;align-items:center}#music-player-section{grid-area:musec;position:relative;background-color:#fff8dc}#empty-drop-zone-text{color:#fff;font-size:2em}.pos-rel{position:relative}.hidden{display:none}.draggable{border:2px dashed #fff}.column{flex-direction:column}.row{flex-direction:row}.full-abs{position:absolute}.centered-abs{position:absolute;top:50%;left:50%;transform:translate(-50%,-50%)}.overlay,.top-left-abs{top:0;left:0;position:absolute}.top-right-abs{position:absolute;top:0;right:0}.flex-1{flex:1}.default-margin{margin:5px}.centered{justify-content:center;align-items:center;text-align:center}.list-contain{display:flex;flex-grow:1;flex-direction:column;justify-content:flex-start}.horiz-list{display:flex;padding-left:0;margin:0;list-style:none}.x-scroll{overflow-x:auto}.square-cell{width:50%;max-width:250px;flex:0 0 auto}.overlay{width:100%;height:100%;background:radial-gradient(ellipse at center,rgba(0,0,0,.5) 0,rgba(0,0,0,0) 100%);opacity:1;transition:opacity .2s}.invisible{opacity:0}.handle{cursor:grab}.hidden-input{border:none;height:0;width:0}.secondary-theme,.user-carousel{background-color:var(--secondary-bg-color)}.tertiary-theme{background-color:var(--tertiary-bg-color)}.primary-icon-stroke{stroke:var(--primary-fnt-color);stroke-width:2;stroke-linecap:round;stroke-linejoin:round}.centered-icon{height:25%;fill:var(--primary-fnt-color)}.warn-btn{background-color:red}.warn-btn:hover{cursor:pointer}.audio-canvas-sm,.full-width{width:100%}.z-0{z-index:0}.z-2{z-index:2}.spaced-hz-li{margin-left:10px}.song-queue-list{list-style-type:none;padding:0;margin:0;display:flex;flex-direction:column;width:100%;counter-reset:item;max-height:250px;overflow-y:auto}.song-list-item{background-color:var(--tertiary-bg-color);padding:10px;margin-bottom:5px;display:flex;align-items:center;justify-content:space-between}.song-title::before{content:counter(item);counter-increment:item;font-weight:700;color:#fff;padding:5px;margin-right:10px}.song-title{display:flex;align-items:center;max-width:225px}.text-ellipsis{white-space:nowrap;overflow:hidden;text-overflow:ellipsis;display:inline-block}.borderless{border:none}.small-btn{width:50px}.warning-hover:hover{fill:var(--warning-color)}.user-carousel-cell{width:200px;height:200px;margin-right:10px;background:var(--tertiary-bg-color);counter-increment:gallery-cell}.lds-facebook-sm{display:inline-block;width:40px;height:40px;position:relative}.lds-facebook-sm div{position:absolute;width:8px;height:32px}.lds-facebook-sm div:first-child{left:4px;animation-delay:-.24s}.lds-facebook-sm div:nth-child(2){left:16px;animation-delay:-.12s}.lds-facebook-sm div:nth-child(3){left:28px;animation-delay:0s}.lds-facebook-md{display:inline-block;position:relative;width:80px;height:80px}.lds-facebook-md div{display:inline-block;position:absolute;left:8px;width:16px}.lds-facebook-md div:first-child{left:8px;animation-delay:-.24s}.lds-facebook-md div:nth-child(2){left:32px;animation-delay:-.12s}.lds-facebook-md div:nth-child(3){left:56px;animation-delay:0}.medium-text{font-size:medium}.big-text{font-size:xx-large}@keyframes lds-facebook{0%{top:4px;height:24px}100%,50%{top:12px;height:12px}}#user-queue>li:first-of-type{border:2px solid gold;animation:2s infinite pulsate}@keyframes pulsate{0%,100%{border-color:gold}50%{border-color:transparent}}"