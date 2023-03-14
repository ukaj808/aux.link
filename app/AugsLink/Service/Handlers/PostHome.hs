module AugsLink.Service.Handlers.PostHome 
  ( create
  ) where

import Servant
import Control.Monad.Cont (MonadIO(liftIO))
import Data.UUID (toText)
import qualified Data.Text as T

import AugsLink.Core.API

create :: Registry IO -> Handler (Headers '[Header "Location" T.Text] T.Text)
create rr = do
  rId <- liftIO $ createRoom rr
  return $ addHeader (genLocation rId) (toText rId)

genLocation :: RoomId -> T.Text
genLocation roomId = T.append (T.pack "http://localhost:8080/") $ toText roomId
