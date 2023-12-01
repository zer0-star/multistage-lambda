module MultLam.Primitive.Types where

import MultLam.Data.Common
import MultLam.Data.Type

primTypes :: [(Name, Type)]
primTypes =
  [ ("add", TArr TInt (TArr TInt TInt))
  , ("sub", TArr TInt (TArr TInt TInt))
  , ("mul", TArr TInt (TArr TInt TInt))
  , ("div", TArr TInt (TArr TInt TInt))
  ]