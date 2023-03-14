{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Core.API where

import           Data.UUID  (UUID)
import           Data.Aeson.Types
import qualified Data.Aeson as Aeson
import Data.Kind (Type)
import qualified Data.Text as T

data Registry m = Registry
  {
     createRoom        ::   m RoomId
  ,  deleteRoom        ::   RoomId     -> m ()
  ,  getRoom           ::   RoomId     -> m (Maybe (Room m))
  ,  numRooms          ::                 m Int
  }

data Room m = Room
  {
     enterRoom             ::   Connection m                         -> m ()
  ,  leaveRoom             ::   UserId                               -> m ()
  ,  presentInRoom         ::                                           m [User]
  ,  currentlyPlaying      ::                                           m SongId
  -- maybe package everyting into "Current RoomState" and return that?
  -- Maybe we need to queue up all the events while a new person is connecting (front end and backend), then process the queue
  }

data UserM m = UserM
 {
    getNextSong     ::   m SongId
 ,  getUserData     ::   m User   
 }

data User = User
 {
    userId :: UserId
 ,  userName :: UserName
 ,  spotInLine     :: Int
 }

data Song = Song
 {
    id     :: SongId 
 ,  title  :: T.Text
 ,  artist :: T.Text
 ,  length :: Int
}

data RoomEvent = UserEnterEvent User
  |              UserLeftEvent  UserId

newtype UserMessage = UserLeftMessage UserId

newtype ServerMessage = ServerWelcomeMessage User

type RoomId   = UUID
type UserId   = UUID
type SongId   = UUID
type UserName = String
type Vote     = Bool

type family Connection (m :: Type -> Type) :: Type
type family EventBus   (m :: Type -> Type) :: Type


instance Eq User where
  u1 == u2 = userId     u1 == userId     u2
instance Ord User where
  u1 <= u2 = spotInLine u1 <= spotInLine u2 

instance ToJSON RoomEvent where
  toJSON :: RoomEvent -> Value
  toJSON (UserEnterEvent u) = Aeson.object 
    [
       "type"        .= ("UserEnterEvent" :: String)
    ,  "userId"      .= userId u
    ,  "userName"    .= userName u
    ,  "spotInLine"  .= spotInLine u
    ]
  toJSON (UserLeftEvent uid) = Aeson.object 
    [
       "type"        .= ("UserLeftEvent"  :: String)
    ,  "userId"      .= uid
    ]
  
instance FromJSON RoomEvent where
  parseJSON :: Value -> Parser RoomEvent
  parseJSON = Aeson.withObject "RoomEvent" $ \obj -> do
      typ <- obj .: "type"
      case typ :: String of
        "UserEnterEvent" -> do
          userId     <- obj .: "userId"
          userName   <- obj .: "userName"
          spotInLine <- obj .: "spotInLine"
          return $ UserEnterEvent $ User userId userName spotInLine
        "UserLeftEvent" -> do
          uid        <- obj .: "userId"
          return $ UserLeftEvent uid

instance ToJSON UserMessage where
  toJSON :: UserMessage -> Value
  toJSON (UserLeftMessage uid) = Aeson.object 
    [
       "type"        .= ("UserLeftMessage" :: String)
    ,  "userId"      .= uid
    ]

instance FromJSON UserMessage where
  parseJSON :: Value -> Parser UserMessage
  parseJSON = Aeson.withObject "UserMessage" $ \obj -> do
      typ <- obj .: "type"
      case typ :: String of
        "UserLeftMessage" -> do
          userId     <- obj .: "userId"
          return $ UserLeftMessage userId

instance ToJSON ServerMessage where
  toJSON :: ServerMessage -> Value
  toJSON (ServerWelcomeMessage u) = Aeson.object 
    [
       "type"        .= ("ServerWelcomeMessage" :: String)
    ,  "userId"      .= userId u
    ,  "userName"    .= userName u
    ,  "spotInLine"  .= spotInLine u
    ]

instance FromJSON ServerMessage where
  parseJSON :: Value -> Parser ServerMessage
  parseJSON = Aeson.withObject "UserMessage" $ \obj -> do
      typ <- obj .: "type"
      case typ :: String of
        "ServerWelcomeMessage" -> do
          userId     <- obj .: "userId"
          userName   <- obj .: "userName"
          spotInLine <- obj .: "spotInLine"
          return $ ServerWelcomeMessage $ User userId userName spotInLine

