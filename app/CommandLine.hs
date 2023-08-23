module CommandLine
  ( 
    CLArgs(..)
  , getCLArgs
  , Environment(..)
  ) where

import Options.Applicative
import Data.String

data Environment = Local | Production
  deriving (Show)

instance IsString Environment where
  fromString "local" = Local
  fromString "production"  = Production
  fromString _             = error "Invalid environment"


data CLArgs =
  CLArgs
    { 
      env             :: Environment
    , publicAssetsPath      :: FilePath
    , homeViewPath    :: FilePath
    , audioWorkerPath :: FilePath
    , roomsDir        :: FilePath
    , roomsDirName    :: FilePath
    }
  deriving (Show)

parseOptions :: Parser CLArgs
parseOptions = CLArgs 
    <$> parseEnv
    <*> parsePublicAssetsPath 
    <*> parseHomeViewPath
    <*> parseAudioWorkerPath
    <*> parseRoomsDir
    <*> parseRoomsDirName

parseEnv :: Parser Environment
parseEnv =
  option str $ mconcat 
    [
      long "env"
    , help "Environment"
    ]

parsePublicAssetsPath :: Parser FilePath
parsePublicAssetsPath =
  option str $ mconcat 
    [
      long "public"
    , help "Directory of the static files"
    ]

parseHomeViewPath :: Parser FilePath
parseHomeViewPath =
  option str $ mconcat 
    [
      long "home"
    , help "Path to the home view (html)"
    ]
    
parseAudioWorkerPath :: Parser FilePath
parseAudioWorkerPath =
  option str $ mconcat 
    [
      long "aw"
    , help "Path to audio worker script (js)"
    ]

parseRoomsDir :: Parser FilePath
parseRoomsDir =
  option str $ mconcat 
    [
      long "roomsDir"
    , help "Path to the rooms directory"
    ]

parseRoomsDirName :: Parser FilePath
parseRoomsDirName =
  option str $ mconcat 
    [
      long "roomsDirName"
    , help "Name of the rooms directory"
    ]

getCLArgs :: IO CLArgs
getCLArgs = execParser $ info (parseOptions <**> helper) fullDesc
