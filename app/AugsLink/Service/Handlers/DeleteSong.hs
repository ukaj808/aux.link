{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Service.Handlers.DeleteSong
  (
    remove
  ) where 

import Data.Text
import Servant

import AugsLink.Core.API

remove :: Registry IO -> Text -> Text -> Text -> Handler Text
remove rr xrid xuid xsid = undefined
