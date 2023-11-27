module MultLam.Parser.Expr where

import Data.Text (Text)
import Data.Text qualified as T

import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as L

import Control.Applicative
import Data.Functor
import MultLam.Data.Common
import MultLam.Data.Expr
import MultLam.Parser.Common
import Text.Megaparsec.Error.Builder qualified as E

reservedWords :: [Text]
reservedWords = ["let", "in"]

expr :: Parser (Expr 'Parsed)
expr = M.label "expression" do
  o <- M.getOffset
  es <- M.some atom
  return $ foldl1 (EApp o) es

atom :: Parser (Expr 'Parsed)
atom = var <|> let_ <|> intLit <|> lam <|> par

par :: Parser (Expr 'Parsed)
par = liftA2 EPar M.getOffset $ parens expr

intLit :: Parser (Expr 'Parsed)
intLit = liftA2 EIntLit M.getOffset $ lexeme (L.signed (return ()) L.decimal)

var' :: Parser LName
var' =
  M.label "variable"
    $ M.try do
      x <- lname
      if x.name `elem` reservedWords
        then M.parseError $ E.err x.offset $ E.ulabel ("reserved word " <> show x.name)
        else return x

var :: Parser (Expr 'Parsed)
var = liftA2 EVar M.getOffset (var' <&> (.name))

lam :: Parser (Expr 'Parsed)
lam = do
  o <- M.getOffset
  symbol' "\\"
  xs <- M.some identifier
  symbol' "->"
  e <- expr
  return $ foldr (ELam o) e xs

let_ :: Parser (Expr 'Parsed)
let_ = do
  o <- M.getOffset
  M.try $ identifier' "let"
  x <- identifier
  symbol' "="
  e1 <- expr
  identifier' "in"
  e2 <- expr
  return $ ELet o x e1 e2
