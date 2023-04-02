{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Handlers.PutMoveSong
  (
    move
  ) where 

import Data.Text
import Servant

import AugsLink.Core.API

move :: Registry IO -> Text -> Text -> Text -> Int -> Handler Text
move rr rId uId sId p = undefined
