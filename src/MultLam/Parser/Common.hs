module MultLam.Parser.Common where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)

import MultLam.Data.Common
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Read.Lex (isSymbolChar)

type Parser = M.Parsec Void Text

sc :: Parser ()
sc = L.space M.space1 lineCmnt blockCmnt
 where
  lineCmnt = L.skipLineComment "--"

  blockCmnt = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Parser Text
symbol = lexeme $ M.takeWhile1P (Just "symbol") isSymbolChar

symbol' :: Text -> Parser Text
symbol' s = lexeme $ M.string s <* M.notFollowedBy (M.satisfy isSymbolChar)

identifier :: Parser Text
identifier = lexeme $ do
  c <- M.letterChar
  cs <- M.many (M.alphaNumChar <|> M.oneOf ['_', '\'', '-'])
  return $ T.pack (c : cs)

identifier' :: Text -> Parser Text
identifier' s = lexeme $ M.string s <* M.notFollowedBy (M.alphaNumChar <|> M.oneOf ['_', '\'', '-'])

lname :: Parser LName
lname = do
  o <- M.getOffset
  x <- identifier
  return $ LName x o

parens :: Parser a -> Parser a
parens = M.between (lexeme "(") (lexeme ")")
