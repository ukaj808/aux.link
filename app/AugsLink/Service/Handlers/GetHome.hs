module AugsLink.Service.Handlers.GetHome 
  ( home
  ) where

import Control.Monad.IO.Class ( liftIO )
import Servant

import AugsLink.Service.API ( StaticHtml (..) )
import CommandLine ( homeViewPath , CLArgs )
import Data.Text.IO as T

home :: CLArgs -> Handler StaticHtml
home opts = do
  homeHtmlFile <- liftIO $ T.readFile $ homeViewPath opts 
  return $ StaticHtml homeHtmlFile
