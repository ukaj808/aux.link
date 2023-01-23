module CommandLine
  ( Options(..)
  , getOptions
  ) where

import Options.Applicative

data Options =
  Options
    { staticFilePath :: FilePath
    }
  deriving (Show)

parseOptions :: Parser Options
parseOptions = Options <$> parseStaticFilePath

parseStaticFilePath :: Parser FilePath
parseStaticFilePath =
  option str $ mconcat [
      long "static"
    , help "Directory of the static files"
    ]

getOptions :: IO Options
getOptions = execParser $ info (parseOptions <**> helper) fullDesc
