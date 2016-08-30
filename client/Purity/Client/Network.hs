{-# LANGUAGE LambdaCase #-}
module Purity.Client.Network where

import Purity.Client.Data
import Purity.Client.Lobby
import Purity.Client.Demo
import Purity.Client.Game
import Purity.Client.Util
import Purity.Client.Parse

import Control.Concurrent
import Control.Monad
import Data.IORef
import qualified Network.WebSockets as WS
import qualified Data.Text as T

startDemo :: MVar Mode -> IO ()
startDemo mvarMode = do
  setMode mvarMode demoMode 
  startDemo mvarMode

connectToServer :: String -> MVar Mode -> IO ()
connectToServer url mvarMode = do
  plog Log $ "Connecting to " ++ url
  -- Make a fresh IORef for lastCursorPosition
  cursorRef <- newIORef (0,0)
  WS.runClient url 9160 "/" (wsClient cursorRef mvarMode)

wsClient :: IORef (Double,Double) -> MVar Mode -> WS.ClientApp ()
wsClient cursorRef mvarMode conn = forever $ do
  msg <- T.unpack <$> WS.receiveData conn 
  --plog Log $ "[SERVER] " ++ msg
  case parseServerMessage msg of
    Left err -> plog Error $ "Oh No! Parse error on server message \"" ++ msg ++ "\"!!!\n" ++ show err
    Right serverMsg ->
     --plog Log $ "Received a " ++ show serverMsg ++ " from the server"
     case serverMsg of
       NameQuery -> sendName conn
       LobbyUpdate entries -> setMode mvarMode (lobbyMode entries conn)
       GameUpdate objs plas -> setMode mvarMode (gameMode cursorRef conn objs plas testWPL)

sendName :: WS.Connection -> IO ()
sendName conn = WS.sendTextData conn . T.pack $ "lazersmoke"

setMode :: MVar Mode -> Mode -> IO ()
setMode mvarMode newMode = modifyMVar_ mvarMode (const $ return newMode)

{-
 - for when i need to expand this definiton beyond a single token
updateLobbyDisplay :: RenderDescriptor -> [LobbyEntry] -> IO ()
updateLobbyDisplay mvarMode entries = putMVar mvarMode entries
-}

