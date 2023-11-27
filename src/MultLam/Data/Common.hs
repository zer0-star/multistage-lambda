module MultLam.Data.Common where

import Data.Text (Text)

type Offset = Int

type Name = Text
data LName = LName {name :: Name, offset :: Offset}
  deriving (Eq)
