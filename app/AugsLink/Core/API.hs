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
  ,  leaveRoom             ::   UserId                               -> m ()
  ,  viewRoom              ::                                           m [RoomUser]
  ,  getUser               ::   UserId                              ->  m (Maybe (User m))
  ,  getMusic              ::                                           m (Music m)
  ,  nextUp                ::                                           m (User m)
  ,  getCreatorId          ::                                           m (Maybe UserId)
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
data User m = User
  {
    enqueueSong       :: Priority ->               m SongId
  , uploadSong        :: SongId   -> SongFile m -> m ()
  , getRoomUser       ::                           m RoomUser
  , removeSong        :: SongId                 -> m ()
  , moveSong          :: SongId   -> Priority   -> m ()
  , dequeueSong       ::                           m (Either String (Maybe SongId)) -- Either failed to upload or user hasnt queued anything...
  }

data Music m = Music
  {
    start         :: Room m -> UserId       -> m ()
  , listen        :: UserId -> Connection m -> m ()
  , stopListening :: UserId                 -> m ()
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
   } deriving (Show)

data SongInfo = SongInfo
  {
     songTitle  :: Text
  ,  songArtist :: Text
  ,  songLength :: Int
  } deriving (Generic, FromJSON, Show)

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
data ServerMessage = ServerWelcomeMessage RoomUser
  |                  ServerUploadSong SongId

type RoomId   = Text
type UserId   = Int
type SongId   = Text
type UserName = Text
type Vote     = Bool

type family Connection (m :: Type -> Type) :: Type
type family SongFile   (m :: Type -> Type) :: Type
type family RawMusicConverterExec (m :: Type -> Type) :: Type

instance Eq RoomUser where
  u1 == u2 = userId u1 == userId u2

instance Ord RoomUser where
  u1 <= u2 = userId u1 <= userId u2


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
    ,  "channel"     .= ("Room" :: Text)
    ,  "userId"      .= uid
    ]

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
  toJSON (ServerUploadSong sId) = Aeson.object
    [
       "type"        .= ("ServerUploadSong" :: Text)
    ,  "songId"      .= sId
    ]
