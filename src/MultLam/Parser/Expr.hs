module MultLam.Parser.Expr where

import           Data.Text (Text)
import qualified Data.Text as T

import qualified Text.Megaparsec as M
import           Text.Megaparsec ((<|>))
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

import           MultLam.Data.Common
import           MultLam.Data.Expr
import           MultLam.Parser.Common

reservedWords :: [Text]
reservedWords = ["let", "in"]

expr :: Parser RawExpr
expr = do
  es <- M.some atom
  return $ foldl1 RApp es

atom :: Parser RawExpr
atom = var <|> let_ <|> intLit <|> lam <|> par

par :: Parser RawExpr
par = RPar <$> parens expr

intLit :: Parser RawExpr
intLit = RIntLit <$> lexeme (L.signed (return ()) L.decimal)

var :: Parser RawExpr
var = M.try $ do
  x <- identifier
  if x `elem` reservedWords
    then fail $ "keyword " ++ show x ++ " cannot be an identifier"
    else return $ RVar x

lam :: Parser RawExpr
lam = do
  symbol' "\\"
  xs <- M.some identifier
  symbol' "->"
  e <- expr
  return $ foldr RLam e xs

let_ :: Parser RawExpr
let_ = do
  M.try $ identifier' "let"
  x <- identifier
  symbol' "="
  e1 <- expr
  identifier' "in"
  e2 <- expr
  return $ RLet x e1 e2