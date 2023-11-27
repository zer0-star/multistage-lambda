module MultLam.Data.Expr where

import Data.Text qualified as T

import MultLam.Data.Common
import MultLam.Data.Type

data Phase = Parsed | Renamed | Typed

data Expr phase
  = EVar Offset (XId phase)
  | EPrim Offset Name
  | ELam Offset (XDecl phase) (Expr phase)
  | EApp Offset (Expr phase) (Expr phase)
  | ELet Offset (XLetDecl phase) (Expr phase) (Expr phase)
  | EPar Offset (Expr phase)
  | EIntLit Offset Int

instance Show (Expr 'Parsed) where
  show (EVar _ x) = T.unpack x
  show (EPrim {}) = error "show: RPrim"
  show (ELam _ xs e) = "\\" ++ T.unpack xs ++ " -> " ++ show e
  show (EApp _ e1 e2) = show e1 ++ " " ++ show e2
  show (ELet _ x e1 e2) = "let " ++ T.unpack x ++ " = " ++ show e1 ++ " in " ++ show e2
  show (EPar _ e) = "(" ++ show e ++ ")"
  show (EIntLit _ i) = show i

instance Show (Expr 'Renamed) where
  show (EVar _ (i, x)) = T.unpack x <> "#" <> show i
  show (EPrim _ x) = T.unpack x <> "#prim"
  show (ELam _ xs e) = "\\" ++ T.unpack xs ++ " -> " ++ show e
  show (EApp _ e1 e2) = show e1 ++ " " ++ show e2
  show (ELet _ x e1 e2) = "let " ++ T.unpack x ++ " = " ++ show e1 ++ " in " ++ show e2
  show (EPar _ e) = "(" ++ show e ++ ")"
  show (EIntLit _ i) = show i

instance Show (Expr 'Typed) where
  show (EVar _ (i, x)) = T.unpack x <> "#" <> show i
  show (EPrim _ x) = T.unpack x <> "#prim"
  show (ELam _ (x, t) e) = "\\" ++ T.unpack x ++ " : " ++ show t ++ " -> " ++ show e
  show (EApp _ e1 e2) = show e1 ++ " " ++ show e2
  show (ELet _ (x, t) e1 e2) = "let " ++ T.unpack x ++ " : " ++ show t ++ " = " ++ show e1 ++ " in " ++ show e2
  show (EPar _ e) = "(" ++ show e ++ ")"
  show (EIntLit _ i) = show i

type family XId (x :: Phase)
type family XDecl (x :: Phase)
type family XLetDecl (x :: Phase)

type instance XId 'Parsed = Name
type instance XId 'Renamed = (Int, Name)
type instance XId 'Typed = (Int, Name)

type instance XDecl 'Parsed = Name
type instance XDecl 'Renamed = Name
type instance XDecl 'Typed = (Name, Type)

type instance XLetDecl 'Parsed = Name
type instance XLetDecl 'Renamed = Name
type instance XLetDecl 'Typed = (Name, Scheme)