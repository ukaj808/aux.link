{-# LANGUAGE OverloadedStrings #-}
module AuxWire.Browser.RoomDesktop (
    styles
) where

import Clay

styles :: Css
styles = do
    (html <> body) ?
        do
        margin nil nil nil nil
    html ?
        do
        display flex
        height (vh 100)
    body ? 
        do
        display grid
        flexGrow 1
        flexShrink 1
        gridTemplateColumns [fr 1, fr 1.5]
        "grid-template-rows" -: "125px 3fr .5fr"
        "grid-template-areas" -: "\"risec musec\" \"usec dropsec\" \"usec dropsec\""
    "#room-info-section" ?
        do
        "grid-area" -: "risec"
        display flex
        justifyContent center
        alignItems center
        backgroundColor (rgb 250 235 215)
    "#user-section" ?
        do
        "grid-area" -: "usec"
        position relative
        overflow hidden
    "#drop-section" ?
        do
        "grid-area" -: "dropsec"
        backgroundColor (rgb 47 79 79)
    "#drop-zone" ?
        do
        flexGrow 1
        flexShrink 1
        display flex
        flexDirection column
        justifyContent center
        alignItems center
        width (pct 100)
        height (pct 100)
    "#music-player-section" ?
        do
        "grid-area" -: "musec"
        position relative
        backgroundColor (rgb 255 248 220)

    ".user" ?
        do
        position absolute
        width (pct 96)
        height (px 100)
        left nil
        right nil
        top nil
        margin auto auto auto auto 
