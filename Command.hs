module Command
  (
    Command (..)
  , parseCommand
  ) where

import Control.Applicative

import Text.ParserCombinators.Parsec hiding ((<|>))

import qualified Data.Text as T


-- | Server admin commands
data Command = SetMotd T.Text
             | ClearMotd
             | Msg Integer T.Text

parseCommand :: String -> Either ParseError Command
parseCommand = parse command "console"

command :: GenParser Char st Command
command = setMotd <|> clearMotd <|> msgClient

setMotd :: GenParser Char st Command
setMotd =
  SetMotd . T.pack <$> (string "SETMOTD" *> spaces *> many1 anyChar)

clearMotd :: GenParser Char st Command
clearMotd = ClearMotd <$ string "CLRMOTD"

msgClient :: GenParser Char st Command
msgClient =
  Msg <$> (string "MSG" *> spaces *> parseClientId)
      <*> fmap T.pack (spaces *> many1 anyChar)

  where
    parseClientId = read <$> many1 digit
