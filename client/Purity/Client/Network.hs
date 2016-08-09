{-# LANGUAGE LambdaCase #-}
module Purity.Client.Network where

import Purity.Client.Data
import Purity.Client.Util
import Purity.Client.Parse

import Data.Access

import Control.Concurrent
import Control.Monad
import qualified Network.WebSockets as WS
import qualified Data.Text as T

connectToServer :: String -> MVar Mode -> IO ()
connectToServer url mvarRender = do
  plog $ "Connecting to " ++ url
  WS.runClient url 9160 "/" (wsClient mvarRender)

wsClient :: MVar Mode -> WS.ClientApp ()
wsClient mvarRender conn = forever $ do
  msg <- T.unpack <$> WS.receiveData conn 
  plog $ "[SERVER] " ++ msg
  case parseServerMessage msg of
    Left err -> plog $ "Oh No! Parse error on server message \"" ++ msg ++ "\"!!!\n" ++ show err
    Right serverMsg -> do
     plog $ "Received a " ++ show serverMsg ++ " from the server"
     case serverMsg of
       NameQuery -> sendName conn
       LobbyUpdate entries -> modifyMVar_ mvarRender (return . set Display (Lobby entries))

sendName :: WS.Connection -> IO ()
sendName conn = WS.sendTextData conn . T.pack $ "lazersmoke"

{-
 - for when i need to expand this definiton beyond a single token
updateLobbyDisplay :: RenderDescriptor -> [LobbyEntry] -> IO ()
updateLobbyDisplay mvarRender entries = putMVar mvarRender entries
-}

