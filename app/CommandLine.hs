module CommandLine
  ( Options(..)
  , getOptions
  ) where

import Options.Applicative

data Options =
  Options
    { staticDirPath :: FilePath
    , homeViewPath :: FilePath
    , roomViewPath :: FilePath
    }
  deriving (Show)

parseOptions :: Parser Options
parseOptions = Options 
    <$> parseStaticDirPath 
    <*> parseHomeViewPath
    <*> parseRoomViewPath

parseStaticDirPath :: Parser FilePath
parseStaticDirPath =
    option str $ mconcat 
      [
        long "static"
      , help "Directory of the static files"
      ]

parseHomeViewPath :: Parser FilePath
parseHomeViewPath =
    option str $ mconcat 
      [
        long "home"
      , help "Path to the home view (html)"
      ]
    
parseRoomViewPath :: Parser FilePath
parseRoomViewPath =
    option str $ mconcat 
      [
        long "room"
      , help "Path to the room view (html)"
      ]

getOptions :: IO Options
getOptions = execParser $ info (parseOptions <**> helper) fullDesc
