module MultLam.Renamer (rename) where

import           Control.Applicative

import           Data.List
import           Data.Text

import           MultLam.Data.Common
import           MultLam.Data.Expr
import           MultLam.Primitive

rename :: RawExpr -> Either Text Expr
rename = rename' []

rename' :: [Name] -> RawExpr -> Either Text Expr
rename' env (RVar x)
  | Just i <- x `elemIndex` env = Right $ Var x i
  | Just _ <- x `lookup` primitives = Right $ Prim x
  | otherwise = Left $ "undefined variable " <> x
rename' env (RLam x e) = Lam x <$> rename' (x:env) e
rename' env (RApp e1 e2) = liftA2 App (rename' env e1) (rename' env e2)
rename' env (RLet x e1 e2) =
  liftA2 (Let x) (rename' env e1) (rename' (x:env) e2)
rename' env (RPar e) = rename' env e
rename' env (RIntLit i) = Right $ IntLit i