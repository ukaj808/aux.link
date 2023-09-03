module CommandLine
  ( 
    CLArgs(..)
  , getCLArgs
  ) where

import Options.Applicative


newtype CLArgs =
  CLArgs
    { 
      staticAssetsPath      :: FilePath
    }
  deriving (Show)

parseOptions :: Parser CLArgs
parseOptions = CLArgs 
    <$> parseStaticAssetsPath 

parseStaticAssetsPath :: Parser FilePath
parseStaticAssetsPath =
  option str $ mconcat 
    [
      long "path-to-static-assets"
    , short 'p'
    , help "Directory of the static files"
    ]

getCLArgs :: IO CLArgs
getCLArgs = execParser $ info (parseOptions <**> helper) fullDesc
