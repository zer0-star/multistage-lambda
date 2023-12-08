module MultLam.TypeCheck where

import Control.Monad.Except
import Control.Monad.RWS.Strict
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import MultLam.Data.Expr
import MultLam.Data.Type
import MultLam.Primitive.Types

type Substitution = IntMap Type

type Env = [Scheme]

type Infer a = RWST Env () Int (Either String) a

runInfer :: Infer a -> Either String a
runInfer m = case evalRWST m [] 0 of
  Left e -> Left e
  Right (a, ()) -> Right a

fvar :: Type -> IntSet
fvar (TArr t1 t2) = fvar t1 <> fvar t2
fvar (TVar i) = IntSet.singleton i
fvar _ = mempty

fvarS :: Scheme -> IntSet
fvarS (Forall _ t) = fvar t

apply :: Substitution -> Type -> Type
apply s (TArr t1 t2) = TArr (apply s t1) (apply s t2)
apply s (TVar i) = case IntMap.lookup i s of
  Just t -> t
  Nothing -> TVar i
apply _ t = t

applyBound :: [Type] -> Type -> Type
applyBound env (TBVar i) = env !! i
applyBound env (TArr t1 t2) = TArr (applyBound env t1) (applyBound env t2)
applyBound _ t = t

applyS :: Substitution -> Scheme -> Scheme
applyS s (Forall n t) = Forall n (apply s t)

applyEnv :: Substitution -> Env -> Env
applyEnv s = map (applyS s)

applyExpr :: Substitution -> Expr 'Typed -> Expr 'Typed
applyExpr _ (EVar o (i, x)) = EVar o (i, x)
applyExpr _ (EPrim o x) = EPrim o x
applyExpr s (ELam o (x, t) e) = ELam o (x, apply s t) (applyExpr s e)
applyExpr s (EApp o e1 e2) = EApp o (applyExpr s e1) (applyExpr s e2)
applyExpr s (ELet o (x, t) e1 e2) = ELet o (x, applyS s t) (applyExpr s e1) (applyExpr s e2)
applyExpr s (EPar o e) = EPar o (applyExpr s e)
applyExpr _ (EIntLit o n) = EIntLit o n

infixr 6 `compose`
compose :: Substitution -> Substitution -> Substitution
compose s1 s2 = IntMap.map (apply s1) s2 <> s1

newTVar :: Infer Type
newTVar = do
  i <- get
  put (i + 1)
  pure (TVar i)

instanciate :: Scheme -> Infer Type
instanciate (Forall n t) = do
  ts <- replicateM n newTVar
  pure $ applyBound ts t

generalize :: Env -> Type -> (Scheme, Substitution)
generalize env t = (Forall (length vs) $ apply s t, s)
 where
  vs = IntSet.toList $ fvar t `IntSet.difference` foldMap fvarS env
  s = IntMap.fromList $ zip vs (map TBVar [0 ..])

occurCheck :: Int -> Type -> Bool
occurCheck i (TVar j) = i == j
occurCheck i (TArr t1 t2) = occurCheck i t1 || occurCheck i t2
occurCheck _ _ = False

unify :: Type -> Type -> Infer Substitution
unify (TArr t1 t2) (TArr t3 t4) = do
  s1 <- unify t1 t3
  s2 <- unify (apply s1 t2) (apply s1 t4)
  pure (s2 `compose` s1)
unify (TVar i) (TVar j) | i == j = pure mempty
unify (TVar i) t
  | occurCheck i t = throwError $ "infinte type: " <> show i <> " ~ " <> show t
  | otherwise = pure (IntMap.singleton i t)
unify t (TVar i) = unify (TVar i) t
unify t1 t2
  | t1 == t2 = pure mempty
  | otherwise = throwError $ "cannot unify " <> show t1 <> " ~ " <> show t2

check :: Expr 'Renamed -> Type -> Infer (Substitution, Expr 'Typed)
check (EVar o (i, x)) t = do
  t' <- asks (!! i) >>= instanciate
  s <- unify t t'
  return (s, EVar o (i, x))
check (EPrim o x) t = do
  let Just t' = x `lookup` primTypes
  s <- unify t =<< instanciate t'
  return (s, EPrim o x)
check (ELam o x e) t = do
  t1 <- newTVar
  t2 <- newTVar
  s <- unify t (TArr t1 t2)
  (s', e') <- local (applyEnv s . (Forall 0 (apply s t1) :)) $ check e (apply s t2)
  return (s' `compose` s, ELam o (x, apply s' $ apply s t1) e')
check (EApp o e1 e2) t = do
  t1 <- newTVar
  (s1, e1') <- check e1 (TArr t1 t)
  (s2, e2') <- local (applyEnv s1) $ check e2 (apply s1 t1)
  return (s2 `compose` s1, EApp o e1' e2')
check (ELet o x e1 e2) t = do
  t1 <- newTVar
  (s1, e1') <- check e1 t1
  env <- ask
  let (t1', s1') = generalize env (apply s1 t1)
  (s2, e2') <- local (applyEnv s1 . (t1' :)) $ check e2 (apply s1 t)
  return (s2 `compose` s1, ELet o (x, t1') (applyExpr s1' e1') e2')
check (EPar o e) t = do
  (s, e') <- check e t
  return (s, EPar o e')
check (EIntLit o n) t = do
  s <- unify TInt t
  return (s, EIntLit o n)

infer :: Expr 'Renamed -> Either String (Expr 'Typed, Scheme)
infer e = runInfer do
  t <- newTVar
  (s, e') <- check e t
  env <- asks (applyEnv s)
  let (t', s') = generalize env $ apply s t
  pure (applyExpr s' $ applyExpr s e', t')