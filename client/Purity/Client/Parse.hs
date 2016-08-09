module Purity.Client.Parse (parseServerMessage) where

import Purity.Client.Data

import Control.Monad
import Text.Parsec
import Text.Parsec.String

parseServerMessage :: String -> Either ParseError ServerMessage
parseServerMessage = parse (nameQuery <|> lobbyUpdate) "Server Message"

nameQuery :: Parser ServerMessage
nameQuery = string "What is your name?" >> return NameQuery

lobbyUpdate :: Parser ServerMessage
lobbyUpdate = do
  void $ string "Lobby Status: "
  entries <- between (char '[') (char ']') (lobbyEntry `sepBy` char ',')
  return $ LobbyUpdate entries

lobbyEntry :: Parser LobbyEntry
lobbyEntry = do
  name <- nameIdent
  void $ string " ("
  ident <- nameIdent
  void $ string ") is "
  ready <- readiness
  return $ mkLobbyEntry name ident ready

readiness :: Parser Bool
readiness = (string "Not Ready" >> return False) <|> (string "Ready" >> return True)

nameIdent :: Parser String
nameIdent = many1 (alphaNum <|> oneOf "_'!")

