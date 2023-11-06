{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Browser.RoomDesktop (
    styles
) where

import Clay

{-
html, body {
  margin: 0;
}

html {
  display: flex;
  height: 100vh;
}

body {
  flex: 1;
  display: grid;
  grid-template-columns: 1fr 1.5fr;
  grid-template-rows: 125px 3fr .5fr;
  grid-template-areas:
      "risec      musec"
      "usec    dropsec"
      "usec    dropsec";
}

#room-info-section {
  grid-area: risec;
  display: flex;
  justify-content: center;
  align-items: center;
  background-color: antiquewhite;
}

#user-section {
  grid-area: usec;
  position: relative;
}

.user {
  width: 96%;
  height: 10%;
  position: absolute;
  left: 0;
  right: 0;
  top: 10px;
  margin: auto;
}

#83ed1135-2f1f-4b4c-bfa2-03aa6fe31700 { background-color: #00dcff; z-index: 0; left: 0px; }

#drop-section {
  grid-area: dropsec;
  background-color: darkslategray;
}

#drop-zone {
display: flex;
justify-content: center;
align-items: center;
width: 100%;
height: 100%;
}

#music-player-section {
  grid-area: musec;
  position: relative;
  background-color: cornsilk;
}

-}

styles :: Css
styles = do
    (html <> body) ?
        do
        margin (px 0) (px 0) (px 0) (px 0)
    html ?
        display flex
    body ? 
        do
        display grid
        flexGrow 1
        flexShrink 1
        gridTemplateColumns [fr 1, fr 1.5]
        "grid-template-rows" -: "100px 5fr 1fr"
        "grid-template-areas" -: "\"risec usec\" \"dropsec dropsec\" \"musec musec\""
    "#room-info-section" ?
        do
        "grid-area" -: "risec"
        display flex
        justifyContent center
        alignItems center
        textAlign center
        fontSize (em 1.5)
        lineHeight (em 2)
        fontWeight bold 
    "#user-section" ?
        do
        "grid-area" -: "usec"
        position relative
        overflow hidden
    "#drop-section" ?
        do
        "grid-area" -: "dropsec"
        display flex
        padding (Clay.rem 1) (Clay.rem 1) (Clay.rem 1) (Clay.rem 1)
    "#drop-zone" ?
        do
        flexGrow 1
        flexShrink 1
        display flex
        flexDirection column
        justifyContent center
        alignItems center
        border (px 1) solid (rgb 243 244 246)
        borderRadius (px 4) (px 4) (px 4) (px 4)
    "#drop-zone-title" ?
        do
        alignSelf flexStart
        padding (Clay.rem 1) (Clay.rem 1) (Clay.rem 1) (Clay.rem 1)
    "#music-player-section" ?
        do
        "grid-area" -: "musec"
        position relative
    ".user" ?
        do
        position absolute
        top (px 0)
        left (px 0)
        bottom (px 0)
        margin auto auto auto auto 