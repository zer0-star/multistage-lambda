module MultLam.Parser.Common where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void (Void)

import qualified Text.Megaparsec as M
import           Text.Megaparsec ((<|>))
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Read.Lex (isSymbolChar)

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
  return $ T.pack (c:cs)

identifier' :: Text -> Parser Text
identifier' s = lexeme
  $ M.string s <* M.notFollowedBy (M.alphaNumChar <|> M.oneOf ['_', '\'', '-'])

parens :: Parser a -> Parser a
parens = M.between (lexeme "(") (lexeme ")")