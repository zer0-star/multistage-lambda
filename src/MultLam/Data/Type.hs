module MultLam.Data.Type where

import MultLam.Data.Common

data Type
  = TInt
  | TArr Type Type
  | TVar Int
  | TFVar Int
  deriving (Eq)

data Scheme = Forall [Name] Type
  deriving (Eq)
