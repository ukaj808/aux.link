module AugsLink.Service.Handlers.GetRoom
  ( room
  ) where

import Control.Monad.IO.Class ( liftIO )
import qualified Data.ByteString.Lazy as Lazy
import Servant

import CommandLine ( CLArgs ( roomViewPath ) )

room :: CLArgs -> String -> Handler Lazy.ByteString
room opts _ = liftIO $ Lazy.readFile $ roomViewPath opts
