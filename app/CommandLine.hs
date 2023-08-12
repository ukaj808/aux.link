module CommandLine
  ( 
    CLArgs(..)
  , getCLArgs
  ) where

import Options.Applicative

data CLArgs =
  CLArgs
    { publicAssetsPath      :: FilePath
    , homeViewPath    :: FilePath
    , audioWorkerPath :: FilePath
    , roomsDir        :: FilePath
    , roomsDirName    :: FilePath
    }
  deriving (Show)

parseOptions :: Parser CLArgs
parseOptions = CLArgs 
    <$> parsePublicAssetsPath 
    <*> parseHomeViewPath
    <*> parseAudioWorkerPath
    <*> parseRoomsDir
    <*> parseRoomsDirName

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
