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
    , staticAssetsPath      :: FilePath
    }
  deriving (Show)

parseOptions :: Parser CLArgs
parseOptions = CLArgs 
    <$> parseEnv
    <*> parseStaticAssetsPath 

parseEnv :: Parser Environment
parseEnv =
  option str $ mconcat 
    [
      long "environment"
    , short 'e'
    , help "Environment"
    ]

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
