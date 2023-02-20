{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Handlers.GetHome 

  ( home
  ) where

import qualified Data.ByteString.Lazy as Lazy
import Servant

import CommandLine ( CLArgs )
import Lucid

htmlResponse :: Html ()
htmlResponse = div_ (do p_ "hello"; p_ "sup")

home :: CLArgs -> Handler Lazy.ByteString
home lc = do
  let html = toHtmlRaw htmlResponse
  return $ renderBS html

