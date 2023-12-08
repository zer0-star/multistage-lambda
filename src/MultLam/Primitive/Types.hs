module MultLam.Primitive.Types where

import MultLam.Data.Common
import MultLam.Data.Type

primTypes :: [(Name, Scheme)]
primTypes =
  [ ("add", toS $ TInt --> TInt --> TInt)
  , ("sub", toS $ TInt --> TInt --> TInt)
  , ("mul", toS $ TInt --> TInt --> TInt)
  , ("div", toS $ TInt --> TInt --> TInt)
  ]