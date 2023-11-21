module MultLam.Primitive where

import           MultLam.Data.Common
import           MultLam.Data.Expr
import           MultLam.Data.Type

type Primive = (Type, [Expr] -> Expr)

primitives :: [(Name, Primive)]
primitives = [ ("add", (TArr TInt (TArr TInt TInt), primAdd))
             , ("sub", (TArr TInt (TArr TInt TInt), primSub))
             , ("mul", (TArr TInt (TArr TInt TInt), primMul))
             , ("div", (TArr TInt (TArr TInt TInt), primDiv))]

primAdd :: [Expr] -> Expr
primAdd [IntLit x, IntLit y] = IntLit (x + y)
primAdd _ = error "primAdd: invalid arguments"

primSub :: [Expr] -> Expr
primSub [IntLit x, IntLit y] = IntLit (x - y)
primSub _ = error "primSub: invalid arguments"

primMul :: [Expr] -> Expr
primMul [IntLit x, IntLit y] = IntLit (x * y)
primMul _ = error "primMul: invalid arguments"

primDiv :: [Expr] -> Expr
primDiv [IntLit x, IntLit y] = IntLit (x `div` y)
primDiv _ = error "primDiv: invalid arguments"