{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Handlers.PutMoveSong
  (
    move
  ) where 

import Data.Text
import Servant

import AugsLink.Core.API

move :: Registry IO -> Text -> Text -> Text -> Handler Text
move rr xrid xuid xsid = undefined
