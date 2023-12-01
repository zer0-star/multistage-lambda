module MultLam.Renamer (rename) where

import Control.Applicative
import Data.List
import Data.Text (Text)
import Data.Void
import MultLam.Data.Common
import MultLam.Data.Expr
import MultLam.Primitive.Types
import Text.Megaparsec (ParseError)
import Text.Megaparsec.Error qualified as E
import Text.Megaparsec.Error.Builder qualified as E

rename :: Expr 'Parsed -> Either (ParseError Text Void) (Expr 'Renamed)
rename = rename' []

rename' :: [Name] -> Expr 'Parsed -> Either (ParseError Text Void) (Expr 'Renamed)
rename' env (EVar o x)
  | Just i <- x `elemIndex` env = Right $ EVar o (i, x)
  | Just _ <- x `lookup` primTypes = Right $ EPrim o x
  | otherwise = Left $ E.errFancy o $ E.fancy $ E.ErrorFail ("undefined variable " <> show x)
rename' _ (EPrim {}) = error "rename': RPrim"
rename' env (ELam o x e) = ELam o x <$> rename' (x : env) e
rename' env (EApp o e1 e2) = liftA2 (EApp o) (rename' env e1) (rename' env e2)
rename' env (ELet o x e1 e2) = liftA2 (ELet o x) (rename' env e1) (rename' (x : env) e2)
rename' env (EPar o e) = EPar o <$> rename' env e
rename' _ (EIntLit o i) = Right $ EIntLit o i
