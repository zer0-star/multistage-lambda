module MultLam.Primitive (
  module MultLam.Primitive,
  module MultLam.Primitive.Types,
) where

import MultLam.Data.Common
import MultLam.Data.IR
import MultLam.Primitive.Types

prim :: Name -> [IR] -> IR
prim "add" = primAdd
prim "sub" = primSub
prim "mul" = primMul
prim "div" = primDiv
prim _ = error "prim: unknown primitive"

primArgLen :: Name -> Int
primArgLen "add" = 2
primArgLen "sub" = 2
primArgLen "mul" = 2
primArgLen "div" = 2
primArgLen _ = error "primArgLen: unknown primitive"

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
