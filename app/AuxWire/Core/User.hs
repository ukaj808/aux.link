module AuxWire.Core.User
  (
    newUser
  ) where


import Control.Concurrent

import qualified Network.WebSockets as WS

import AuxWire.Core.API
import Data.UUID.V4
import Data.UUID
import Data.Colour.Palette.RandomColor (randomCIELab)
import Data.Colour.SRGB

import qualified Data.Text as T

type instance Connection IO = WS.PendingConnection

newtype UserState = UserState
  {
    userData        :: RoomUser
  }

newUser :: IO (User IO)
newUser = do
  uuid <- T.pack . ("X" ++) . toString <$> nextRandom
  hex <- T.pack <$> ((sRGB24shows <$> randomCIELab) <*> pure "")
  stateVar <- newMVar $ UserState (RoomUser uuid uuid hex)
  return $ User {
    getRoomUser = userData <$> readMVar stateVar
  }
