{-# LANGUAGE LambdaCase #-}
module Purity.Client.Parse (parseServerMessage,testWPL) where

import Purity.Client.Data

import System.IO.Unsafe
import Data.Maybe
import Control.Monad
import Text.Parsec
import Text.Parsec.String

testWPL :: [RenderPlane]
{-# NOINLINE testWPL #-}
testWPL = unsafePerformIO $ parseFromFile wplFileToRenderPlanes "server/testlevel.wpl" >>= \case
  Right rpl -> return rpl
  Left err -> print err >> return []

parseServerMessage :: String -> Either ParseError ServerMessage
parseServerMessage = parse (nameQuery <|> lobbyUpdate <|> gameUpdate) "Server Message"

nameQuery :: Parser ServerMessage
nameQuery = string "What is your name?" >> return NameQuery

gameUpdate :: Parser ServerMessage
gameUpdate = do
  void $ string "Objects "
  objs <- listOf objectUpdate
  void $ string "\nPlayers "
  plas <- listOf playerUpdate
  return $ GameUpdate objs plas

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

listOf :: Parser a -> Parser [a]
listOf q = between (char '[') (char ']') (q `sepBy` char ',')

wplFileToRenderPlanes :: Parser [RenderPlane]
wplFileToRenderPlanes = do
  wpl <- (fmap Just renderPlane <|> (comment >> return Nothing)) `endBy` endOfLine
  eof
  return . catMaybes $ wpl

comment :: Parser () 
comment = void $ between (char ':') (char ';') (many (alphaNum <|> oneOf "+-/*" <|> space)) 


playerUpdate :: Parser (String,Object)
playerUpdate = do
  plaIdent <- nameIdent
  void $ string ": "
  plaObj <- objectUpdate
  return (plaIdent, plaObj)

objectUpdate :: Parser Object
objectUpdate = do
  size <- vector
  void $ char '@'
  pos <- vector
  void $ string "->"
  dir <- vector
  return $ Object pos size dir

renderPlane :: Parser RenderPlane 
renderPlane = do
  [x,y,z] <- count 3 vector 
  optional comment
  return $ RenderPlane x y z

vector :: Parser Vector
vector = do
  void $ char '<'
  x <- number
  void $ char ','
  y <- number
  void $ char ','
  z <- number
  void $ char '>'
  return $ Vector x y z

number :: Parser Double
number = do
  sign <- anyChar
  num <- many1 digit
  _ <- optional (char '.')
  numTwo <- many digit
  return . (if sign == '-' then negate else id) . (read :: String -> Double) $ num ++ numTwo
