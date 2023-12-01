module MultLam.Data.Expr.Lower where

import Data.Semigroup
import MultLam.Data.Expr
import MultLam.Data.IR
import MultLam.Primitive

lower :: Expr 'Typed -> IR
lower (EVar _ (i, _)) = Var i
lower (EPrim _ f) =
  let n = primArgLen f
   in appEndo (stimes n $ Endo Lam) $ PrimCall f $ reverse $ map Var [0 .. n - 1]
lower (ELam _ _ e) = Lam (lower e)
lower (EApp _ e1 e2) = App (lower e1) (lower e2)
lower (ELet _ _ e1 e2) = App (Lam (lower e2)) (lower e1)
lower (EPar _ e) = lower e
lower (EIntLit _ i) = IntLit i
