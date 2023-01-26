module AugsLink.Service.Application ( server ) where

import Servant 

import Control.Monad.IO.Class
import Data.ByteString.Lazy as Lazy
import Network.WebSockets.Connection

import AugsLink.Service.API (API, RawHtml(..) )
import CommandLine (Options (staticDirPath, homeViewPath, roomViewPath))
import Control.Monad (forever)

handlers :: Options -> Server API
handlers opts = 
         home
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

    -- copy job; need to understand more
    join id pc = liftIO $ do -- Join a Room kept in memory returning an accepted websocket connection
      conn <- liftIO $ acceptRequest pc
      withPingThread conn 30 postPingAction (forever interactWithRoom)
      where 
        postPingAction :: IO ()
        postPingAction = print "ping"

        interactWithRoom :: IO ()
        interactWithRoom = do
          print "hey"

    public = serveDirectoryWebApp $ staticFilePath opts


-- todo
createRoom :: IO String -- creates an empty room in state
createRoom = return "123";

genLocation :: String -> String
genLocation roomId = "http://localhost:8080/" ++ roomId

server :: Options -> Application
server opts = serve (Proxy @API) (handlers opts)
