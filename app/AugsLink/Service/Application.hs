module AugsLink.Service.Application ( server ) where

import Servant 

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as Lazy

import AugsLink.Service.API (API, RawHtml(..) )
import CommandLine (Options (staticFilePath, homeFilePath), roomFilePath)

handlers :: Options -> Server API
handlers opts = home
           :<|> create
           :<|> room 
           :<|> join
           :<|> public

  where 

    home = do
      homeHtmlFile <- liftIO $ Lazy.readFile $ homeFilePath opts 
      return $ RawHtml homeHtmlFile
    
    create = do
      roomId <- liftIO createRoom
      return $ addHeader (genLocation roomId) roomId

    room _ = do
      roomHtmlFile <- liftIO $ Lazy.readFile $ roomFilePath opts
      return $ RawHtml roomHtmlFile

    join id = undefined

    public = serveDirectoryWebApp $ staticFilePath opts

-- todo
createRoom :: IO String
createRoom = return "123";

genLocation :: String -> String
genLocation roomId = "http://localhost:8080/" ++ roomId

server :: Options -> Application
server opts = serve (Proxy @API) (handlers opts)
