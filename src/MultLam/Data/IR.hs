module MultLam.Data.IR where

import Data.List (intercalate)
import Data.Text qualified as T
import MultLam.Data.Common

data IR
  = Var Int
  | PrimCall Name [IR]
  | Lam IR
  | App IR IR
  | IntLit Int

instance Show IR where
  show (Var i) = "#" ++ show i
  show (PrimCall f vs) = T.unpack f ++ "(" ++ intercalate ", " (map show vs) ++ ")"
  show (Lam e) = "(λ. " ++ show e ++ ")"
  show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (IntLit i) = show i
