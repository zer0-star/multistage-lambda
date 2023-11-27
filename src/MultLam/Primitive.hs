module MultLam.Primitive where

import MultLam.Data.Common
import MultLam.Data.IR
import MultLam.Data.Type

type Primive = (Type, [IR] -> IR)

primitives :: [(Name, Primive)]
primitives =
  [ ("add", (TArr TInt (TArr TInt TInt), primAdd))
  , ("sub", (TArr TInt (TArr TInt TInt), primSub))
  , ("mul", (TArr TInt (TArr TInt TInt), primMul))
  , ("div", (TArr TInt (TArr TInt TInt), primDiv))
  ]

primAdd :: [IR] -> IR
primAdd [IntLit x, IntLit y] = IntLit (x + y)
primAdd _ = error "primAdd: invalid arguments"

primSub :: [IR] -> IR
primSub [IntLit x, IntLit y] = IntLit (x - y)
primSub _ = error "primSub: invalid arguments"

primMul :: [IR] -> IR
primMul [IntLit x, IntLit y] = IntLit (x * y)
primMul _ = error "primMul: invalid arguments"

primDiv :: [IR] -> IR
primDiv [IntLit x, IntLit y] = IntLit (x `div` y)
primDiv _ = error "primDiv: invalid arguments"
