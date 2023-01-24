module CommandLine
  ( Options(..)
  , getOptions
  ) where

import Options.Applicative

data Options =
  Options
    { staticFilePath :: FilePath
    , homeFilePath :: FilePath
    , roomFilePath :: FilePath
    }
  deriving (Show)

parseOptions :: Parser Options
parseOptions = Options <$> parseStaticFilePath <*> parseHomeRoute <*> parseRoomRoute

parseStaticFilePath :: Parser FilePath
parseStaticFilePath =
  option str $ mconcat [
      long "static"
    , help "Directory of the static files"
    ]

parseHomeRoute :: Parser FilePath
parseHomeRoute =
  option str $ mconcat [
      long "home"
    , help "path to home html file"
    ]
    
parseRoomRoute :: Parser FilePath
parseRoomRoute =
  option str $ mconcat [
      long "room"
    , help "path to room html file"
    ]

getOptions :: IO Options
getOptions = execParser $ info (parseOptions <**> helper) fullDesc
