{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Core.API where

import Data.Aeson.Types
import Data.Kind
import Data.Text

import qualified Data.Aeson as Aeson
import Servant.Multipart.API

{-
 The Registry monadic interface. This datatype abstracts the actions that the registry 
 can perform on any data type m. It's more useful to look at this data type as 
 a description of what the Registry can do in this program. The registry is responsible
 for the 'management' of rooms. It can create rooms, delete rooms, give you access to a room.
 Think of this as sortve the manager for a hotel.
-}
data Registry m = Registry
  {
     createRoom        ::   m RoomId
  ,  deleteRoom        ::   RoomId     -> m ()
  ,  getRoom           ::   RoomId     -> m (Maybe (Room m))
  ,  numRooms          ::                 m Int
  }

{-
 The Room monadic interface. This data type abstracts the actions that a room
 can perform on any data type m. It's more useful to look at this data type as a 
 description of what the Room can do in this program. The room is responsible
 for the internals of a room. It allows users to enter or leave, it can give
 you the current state of the internals of the room, it manages the song queue
 of the room, it manages the current vote on the current song, and it tells you
 what the current song is. Think of this as a sortve maitre d' for a restaurant
 except every room in our program gets there own maitre d.
 -}
data Room m = Room
  {
     enterRoom             ::   Connection m                         -> m ()
  ,  leaveRoom             ::   UserId                               -> m ()
  ,  viewRoom              ::                                           m [RoomUser]
  ,  getUser               ::   UserId                               -> m (Maybe (User m))
  ,  getMusic              ::                                           m (MusicStreamer m)
  ,  startMusic            ::  UserId                                -> m ()
  ,  uploadSong            ::  UserId -> Upload m                    -> m ()
  -- maybe package everyting into "Current RoomState" and return that?
  -- Maybe we need to queue up all the events while a new person is connecting (front end and backend), then process the queue
  }

{-
 The User monadic interface. This data type abstracts the actions that a User
 can perform on any data type m. It's more useful to look at this data type as a 
 description of what the User can do in this program. The user is responsible
 for what a user can do in a room. You can modify your song queue, you expose the next
 song in your queue for consumption by the room song player, you can vote on skipping a song.
 -}
newtype User m = User {getRoomUser :: m RoomUser}

data Upload m = DirectFileUpload (String, MultipartResult Tmp) | UrlScrapeUpload Text

data MusicStreamer m = Music
  {
    stream             :: FilePath        -> RoomId       -> m  ()
  , connect             :: UserId         -> Connection m -> m  ()
  }

data RoomUser = RoomUser
   {
     userId         :: UserId
   , userName       :: UserName
   , isCreator      :: Bool
   }

data AudioFile = AudioFile
  {
     fileName        :: Text
  ,  tmpPath         :: FilePath
  } deriving (Show)

data Message = RoomEventMessage RoomEvent
  |            ServerCommandMessage ServerCommand

data RoomEvent = UserEnterEvent      RoomUser
  |              UserLeftEvent       UserId
  |              SongStartingEvent   Int
  |              SongUploadedEvent

-- Message from server to user
data ServerCommand = ServerWelcomeCommand RoomUser
  |                  ServerUploadSongCommand

newtype UserEvent = UserAudioPrepared UserId

type RoomId   = Text
type UserId   = Int
type SongId   = Text
type UserName = Text
type Vote     = Bool

type family Connection (m :: Type -> Type) :: Type
type family SongFile   (m :: Type -> Type) :: Type

instance Eq RoomUser where
  u1 == u2 = userId u1 == userId u2

instance Ord RoomUser where
  u1 <= u2 = userId u1 <= userId u2

instance ToJSON Message where
  toJSON :: Message -> Value
  toJSON (RoomEventMessage e) = toJSON e
  toJSON (ServerCommandMessage c) = toJSON c


instance ToJSON RoomEvent where
  toJSON :: RoomEvent -> Value
  toJSON (UserEnterEvent u) = Aeson.object
    [
       "type"        .= ("UserEnterEvent" :: Text)
    ,  "userId"      .= show (userId u)
    ,  "userName"    .= userName u
    ]
  toJSON (UserLeftEvent uid) = Aeson.object
    [
       "type"        .= ("UserLeftEvent"  :: Text)
    ,  "channel"     .= ("Room" :: Text)
    ,  "userId"      .= show uid
    ]
  toJSON (SongStartingEvent s) = Aeson.object
    [
       "type"        .= ("SongStartingEvent"  :: Text)
    ,  "s"      .= s
    ]
  toJSON SongUploadedEvent = Aeson.object
    [
       "type"        .= ("SongUploadedEvent"  :: Text)
    ]

instance ToJSON ServerCommand where
  toJSON :: ServerCommand -> Value
  toJSON (ServerWelcomeCommand u) = Aeson.object
    [
       "type"        .= ("ServerWelcomeCommand" :: Text)
    ,  "userId"      .= show (userId u)
    ,  "userName"    .= userName u
    ,  "isCreator"   .= isCreator u
    ]
  toJSON ServerUploadSongCommand = Aeson.object
    [
       "type"        .= ("ServerUploadSongCommand" :: Text)
    ]