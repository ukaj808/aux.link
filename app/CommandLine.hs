module CommandLine
  ( 
    CLArgs(..)
  , getCLArgs
  ) where

import Options.Applicative

data CLArgs =
  CLArgs
    { staticDirPath :: FilePath
    , homeViewPath :: FilePath
    , audioWorkerPath :: FilePath
    }
  deriving (Show)

parseOptions :: Parser CLArgs
parseOptions = CLArgs 
    <$> parseStaticDirPath 
    <*> parseHomeViewPath
    <*> parseAudioWorkerPath

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
    
parseAudioWorkerPath :: Parser FilePath
parseAudioWorkerPath =
  option str $ mconcat 
    [
      long "aw"
    , help "Path to audio worker script (js)"
    ]

getCLArgs :: IO CLArgs
getCLArgs = execParser $ info (parseOptions <**> helper) fullDesc
