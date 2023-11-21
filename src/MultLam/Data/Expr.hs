module MultLam.Data.Expr where

import Data.Text qualified as T

import MultLam.Data.Common

data RawExpr
  = RVar Name
  | RLam Name RawExpr
  | RApp RawExpr RawExpr
  | RLet Name RawExpr RawExpr
  | RPar RawExpr
  | RIntLit Int

data Expr
  = Var Name Int
  | FVar Name Int
  | Prim Name
  | Lam Name Expr
  | App Expr Expr
  | Let Name Expr Expr
  | IntLit Int

instance Show RawExpr where
  show (RVar x) = T.unpack x
  show (RLam x e) = "\\" ++ T.unpack x ++ " -> " ++ show e
  show (RApp e1 e2) = show e1 ++ " " ++ show e2
  show (RLet x e1 e2) =
    "let " ++ T.unpack x ++ " = " ++ show e1 ++ " in " ++ show e2
  show (RPar e) = "(" ++ show e ++ ")"
  show (RIntLit i) = show i

instance Show Expr where
  show (Var x i) = T.unpack x ++ "#" ++ show i
  show (FVar x _) = T.unpack x ++ "#free"
  show (Prim x) = T.unpack x
  show (Lam x e) = "(λ" ++ T.unpack x ++ ". " ++ show e ++ ")"
  show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Let x e1 e2) =
    "(let " ++ T.unpack x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ")"
  show (IntLit i) = show i
