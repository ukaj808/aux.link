{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Browser.RoomMobile (
    styles
) where

import Clay

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
      "grid-template-area" -: "\"risec usec\" \"dropsec dropsec\" \"musec musec\""
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
      height (px 40)
      width (px 40)
      borderRadius (pct 50) (pct 50) (pct 50) (pct 50)
