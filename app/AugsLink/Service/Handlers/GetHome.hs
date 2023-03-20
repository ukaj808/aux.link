module AugsLink.Service.Handlers.GetHome 
  ( 
    home
  ) where

import Control.Monad.IO.Class
import Servant

import AugsLink.Service.API
import CommandLine
import Data.Text.IO as T

home :: CLArgs -> Handler StaticHtml
home opts = do
  homeHtmlFile <- liftIO $ T.readFile $ homeViewPath opts 
  return $ StaticHtml homeHtmlFile
