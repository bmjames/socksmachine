module SocksMachine.Command
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
             | Announce T.Text
             | Msg Integer T.Text

parseCommand :: String -> Either ParseError Command
parseCommand = parse command "console"

command :: GenParser Char st Command
command = setMotd <|> clearMotd <|> announce <|> msg

setMotd :: GenParser Char st Command
setMotd =
  SetMotd . T.pack <$> (string "SETMOTD" *> spaces *> many1 anyChar)

clearMotd :: GenParser Char st Command
clearMotd = ClearMotd <$ string "CLRMOTD"

announce :: GenParser Char st Command
announce = Announce . T.pack <$> (string "ANN" *> spaces *> many1 anyChar)

msg :: GenParser Char st Command
msg = Msg <$> (string "MSG" *> spaces *> parseClientId)
          <*> fmap T.pack (spaces *> many1 anyChar)

  where
    parseClientId = read <$> many1 digit
