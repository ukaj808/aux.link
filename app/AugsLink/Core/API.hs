{-# LANGUAGE OverloadedStrings #-}
module AugsLink.Core.API where

import Data.Aeson.Types
import Data.Kind
import Data.Text
import GHC.Generics

import qualified Data.Aeson as Aeson
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
  ,  leaveRoom             ::   (UserId, UserId)                               -> m ()
  ,  viewRoom              ::                                           m RoomView
  ,  getUser               ::   UserId                               -> m (Maybe (User m))
  ,  getMusic              ::                                           m (MusicStreamer m)
  ,  startMusic            ::  UserId                                -> m StartMusicResult
  ,  uploadSong            ::  UserId -> Upload                      -> m Bool
  ,  getRoomPath           ::                                           m FilePath
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

data Upload = Upload {
  uploadName :: Text,
  uploadTmp :: FilePath
}

data MusicStreamer m = Music
  {
    stream             :: FilePath        -> RoomId       -> m  ()
  , connect            :: Connection m                    -> m  ()
  }

data StartMusicResult = StartMusicSuccess | NotCreator | AlreadyRunning | RoomStillCreating deriving (Show, Eq)

data MusicState = Streaming | Countdown | Polling | NotRunning deriving (Show, Eq, Generic)
instance ToJSON MusicState


newtype UserQueueView = UserQueue
  {
     uqvQueue :: [RoomUser]
  }

data CurrentlyPlayingView = CurrentlyPlaying
  {
     cpvSong   :: Maybe SongId
  ,  cpvState  :: MusicState
  ,  cpvCountdown :: Maybe Int
  }

-- A sanitized view of the room
data RoomView = RoomView
  {
     cpv :: CurrentlyPlayingView
  ,  uqv  :: UserQueueView
  ,  rId  :: RoomId
  }

data RoomUser = RoomUser
   {
      sanitizedUserId :: UserId
   ,  userName        :: UserName
   ,  hexColor           :: Text
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
  |              MusicStartedEvent
  |              NextInQueueEvent
  |              CountingDownEvent   Int
  |              SongUploadedEvent   Text
  |              SongUploadTimeoutEvent

-- Message from server to user
data ServerCommand = ServerWelcomeCommand UserId Text Bool
  |                  ServerUploadSongCommand

newtype UserEvent = UserAudioPrepared UserId

type RoomId   = Text
type UserId   = Text
type SongId   = Text
type UserName = Text
type Vote     = Bool

type family Connection (m :: Type -> Type) :: Type
type family SongFile   (m :: Type -> Type) :: Type

instance ToJSON Message where
  toJSON :: Message -> Value
  toJSON (RoomEventMessage e) = toJSON e
  toJSON (ServerCommandMessage c) = toJSON c


instance ToJSON RoomEvent where
  toJSON :: RoomEvent -> Value
  toJSON (UserEnterEvent u) = Aeson.object
    [
       "type"        .= ("UserEnterEvent" :: Text)
    ,  "userId"      .= sanitizedUserId u
    ,  "userName"    .= userName u
    ,  "hexColor"       .= hexColor u
    ]
  toJSON (UserLeftEvent uid) = Aeson.object
    [
       "type"        .= ("UserLeftEvent"  :: Text)
    ,  "userId"      .= uid
    ]
  toJSON MusicStartedEvent = Aeson.object
    [
       "type"        .= ("MusicStartedEvent"  :: Text)
    ]
  toJSON NextInQueueEvent = Aeson.object
    [
       "type"        .= ("NextInQueueEvent"  :: Text)
    ]
  toJSON (CountingDownEvent s) = Aeson.object
    [
       "type"        .= ("CountingDownEvent"  :: Text)
    ,  "s"      .= s
    ]
  toJSON (SongUploadedEvent songTitle) = Aeson.object
    [
       "type"        .= ("SongUploadedEvent"  :: Text)
    ,  "title"       .= songTitle
    ]
  toJSON SongUploadTimeoutEvent = Aeson.object
    [
       "type"        .= ("SongUploadTimeoutEvent"  :: Text)
    ] 

instance ToJSON ServerCommand where
  toJSON :: ServerCommand -> Value
  toJSON (ServerWelcomeCommand uId hexColor creator) = Aeson.object
    [
       "type"        .= ("ServerWelcomeCommand" :: Text)
    ,  "userId"      .= uId
    ,  "hexColor"    .= hexColor
    ,  "isCreator"   .= creator
    ]
  toJSON ServerUploadSongCommand = Aeson.object
    [
       "type"        .= ("ServerUploadSongCommand" :: Text)
    ]

instance ToJSON RoomUser where
  toJSON :: RoomUser -> Value
  toJSON u = Aeson.object
    [
       "userName"    .= userName u
    ]

instance ToJSON CurrentlyPlayingView where
  toJSON :: CurrentlyPlayingView -> Value
  toJSON cpv = Aeson.object
    [
       "song"       .= cpvSong cpv
    ,  "musicState" .= cpvState cpv
    ,  "countdown"  .= cpvCountdown cpv
    ]

instance ToJSON UserQueueView where
  toJSON :: UserQueueView -> Value
  toJSON qv = Aeson.object
    [
       "queue"               .= uqvQueue qv
    ]

instance ToJSON RoomView where
  toJSON :: RoomView -> Value
  toJSON rv = Aeson.object
    [
       "currentlyPlayingView"    .= cpv rv
    ,  "queueView"               .= uqv rv
    ]