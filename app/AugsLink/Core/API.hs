{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Core.API where

import Data.UUID
import Data.Aeson.Types
import Data.Kind
import Data.Text

import qualified Data.Aeson as Aeson

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
  ,  viewRoom              ::                                           m [RoomUser]
  ,  currentlyPlaying      ::                                           m SongId
  ,  getUser               ::   UserId  ->                              m (Maybe (User m))
  ,  uploadSong            ::   SongId  -> SongFile m ->                m ()
  -- maybe package everyting into "Current RoomState" and return that?
  -- Maybe we need to queue up all the events while a new person is connecting (front end and backend), then process the queue
  }

data User m = User
  {
    enqueueSong :: SongInfo -> Priority -> m SongId
  , getRoomUser ::                         m RoomUser
  , getNextSong ::                         m Song
  , removeSong  :: SongId               -> m ()
  --, uploadSong  :: SongId -> SongFile m -> m ()
  }
  
data RoomUser = RoomUser
   {
     userId         :: UserId
   , userName       :: UserName
   }

data Song = Song
   {
     songId       :: SongId 
   , songInfo     :: SongInfo
   }

data SongInfo = SongInfo
  {
     songTitle  :: Text
  ,  songArtist :: Text
  ,  songLength :: Int
  }

-- Event published from room to users but also published from the users browser solely to the Room
data RoomEvent = UserEnterEvent RoomUser -- maybe reuse this instead of UserLeftMessage..
  |              UserLeftEvent  UserId

type Priority = Int

-- Command from User to Server
data UserCommand = RemoveSong  SongId
  |                EnqueueSong SongInfo Priority

-- Command from Server to user
newtype ServerCommand = UploadSong SongId

-- Message from server to user
newtype ServerMessage = ServerWelcomeMessage RoomUser

type RoomId   = UUID
type UserId   = UUID
type SongId   = UUID
type UserName = Text
type Vote     = Bool

type family Connection (m :: Type -> Type) :: Type
type family SongFile   (m :: Type -> Type) :: Type

instance Eq RoomUser where
  u1 == u2 = userId     u1 == userId     u2

instance ToJSON RoomEvent where
  toJSON :: RoomEvent -> Value
  toJSON (UserEnterEvent u) = Aeson.object 
    [
       "type"        .= ("UserEnterEvent" :: Text)
    ,  "userId"      .= userId u
    ,  "userName"    .= userName u
    ]
  toJSON (UserLeftEvent uid) = Aeson.object 
    [
       "type"        .= ("UserLeftEvent"  :: Text)
    ,  "userId"      .= uid
    ]
  
instance FromJSON RoomEvent where
  parseJSON :: Value -> Parser RoomEvent
  parseJSON = Aeson.withObject "RoomEvent" $ \obj -> do
      typ <- obj .: "type"
      case typ :: Text of
        "UserEnterEvent" -> do
          userId     <- obj .: "userId"
          userName   <- obj .: "userName"
          return $ UserEnterEvent $ RoomUser userId userName
        "UserLeftEvent" -> do
          uid        <- obj .: "userId"
          return $ UserLeftEvent uid

instance ToJSON SongInfo where
  toJSON :: SongInfo -> Value
  toJSON (SongInfo sTitle sArtist sLength) = Aeson.object 
    [
      "title"      .= sTitle
    , "artist"     .= sArtist
    , "length"     .= sLength
    ]


instance ToJSON ServerMessage where
  toJSON :: ServerMessage -> Value
  toJSON (ServerWelcomeMessage u) = Aeson.object 
    [
       "type"        .= ("ServerWelcomeMessage" :: Text)
    ,  "userId"      .= userId u
    ,  "userName"    .= userName u
    ]

instance FromJSON ServerMessage where
  parseJSON :: Value -> Parser ServerMessage
  parseJSON = Aeson.withObject "UserMessage" $ \obj -> do
      typ <- obj .: "type"
      case typ :: Text of
        "ServerWelcomeMessage" -> do
          userId     <- obj .: "userId"
          userName   <- obj .: "userName"
          return $ ServerWelcomeMessage $ RoomUser userId userName
