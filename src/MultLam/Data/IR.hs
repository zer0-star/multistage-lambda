module MultLam.Data.IR where

import Data.Text qualified as T
import MultLam.Data.Common

data IR
  = Var Name Int
  | FVar Name Int
  | Prim Name
  | Lam Name IR
  | App IR IR
  | Let Name IR IR
  | IntLit Int

instance Show IR where
  show (Var x i) = T.unpack x ++ "#" ++ show i
  show (FVar x _) = T.unpack x ++ "#free"
  show (Prim x) = T.unpack x
  show (Lam x e) = "(λ" ++ T.unpack x ++ ". " ++ show e ++ ")"
  show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Let x e1 e2) = "(let " ++ T.unpack x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ")"
  show (IntLit i) = show i
