module MultLam.Evaluate (eval) where

import Control.Monad.Except
import MultLam.Data.IR
import MultLam.Primitive (prim)

-- type EvalM a = RWST Env () Int (Either String) a
type EvalM = Except String

open :: IR -> IR -> IR
open u = go 0
 where
  go :: Int -> IR -> IR
  go d (Var i) = if i == d then u else Var i
  go d (PrimCall f vs) = PrimCall f (map (go d) vs)
  go d (Lam e) = Lam (go (d + 1) e)
  go d (App e1 e2) = App (go d e1) (go d e2)
  go _ e = e

eval' :: IR -> EvalM IR
eval' (Var _) = throwError "variable cannot be evaluated directly"
eval' (PrimCall f vs) = prim f <$> mapM eval' vs
eval' (Lam e) = return (Lam e)
eval' (App e1 e2) = do
  v1 <- eval' e1
  v2 <- eval' e2
  case v1 of
    Lam e -> eval' $ open v2 e
    _ -> throwError "cannot apply non-lambda"
eval' (IntLit i) = return (IntLit i)

eval :: IR -> Either String IR
eval e = runExcept (eval' e)