module CommandLine
  ( 
    CLArgs(..)
  , getCLArgs
  ) where

import Options.Applicative


data CLArgs =
  CLArgs
    { 
      staticAssetsPath      :: FilePath
    , certificatePath       :: FilePath
    , privateKeyPath        :: FilePath
    }
  deriving (Show)

parseOptions :: Parser CLArgs
parseOptions = CLArgs 
    <$> parseStaticAssetsPath 
    <*> parseCertificatePath
    <*> parsePrivateKeyPath

parseStaticAssetsPath :: Parser FilePath
parseStaticAssetsPath =
  option str $ mconcat 
    [
      long "path-to-static-files"
    , short 'p'
    , help "Directory of the static files"
    ]

parseCertificatePath :: Parser FilePath
parseCertificatePath =
  option str $ mconcat 
    [
      long "certificate-path"
    , short 'c'
    , help "Path to the certificate"
    ]

parsePrivateKeyPath :: Parser FilePath
parsePrivateKeyPath =
  option str $ mconcat 
    [
      long "private-key-path"
    , short 'k'
    , help "Path to the private key"
    ]

getCLArgs :: IO CLArgs
getCLArgs = execParser $ info (parseOptions <**> helper) fullDesc
