module AugsLink.Service.Handlers.GetHome 
  ( 
    homeHandler
  ) where

import Control.Monad.IO.Class
import Servant

import Data.Text.IO as T

import AugsLink.Service.API
import CommandLine

import System.FilePath

homeHandler :: CLArgs -> Handler StaticHtml
homeHandler opts = do
  homeHtmlFile <- liftIO $ T.readFile homeViewFilePath
  return $ StaticHtml homeHtmlFile
  where
    homeViewFilePath = staticAssetsPath opts </> "home.html"