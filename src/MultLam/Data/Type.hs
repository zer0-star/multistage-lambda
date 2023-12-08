module MultLam.Data.Type where

data Type
  = TInt
  | TArr Type Type
  | TVar Int
  | TBVar Int
  deriving (Eq)

infixr 3 -->
(-->) :: Type -> Type -> Type
(-->) = TArr

data Scheme = Forall Int Type
  deriving (Eq)

toS :: Type -> Scheme
toS = Forall 0

instance Show Type where
  show TInt = "Int"
  show (TArr t1 t2) = "(" <> show t1 <> " -> " <> show t2 <> ")"
  show (TVar i) = "'" <> show i
  show (TBVar i) = "#" <> show i

instance Show Scheme where
  show (Forall 0 t) = show t
  show (Forall n t) = "forall (" <> show n <> "). " <> show t