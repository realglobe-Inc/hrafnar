module Hrafnar.Recipe.Annotation
  ( Located(..)
  , toPos
  , toValue
  , Position(..)
  , withDummy
  ) where

import           Data.Functor (Functor)

data Located a = At Position a deriving (Eq)

instance Functor Located where
  fmap f (At pos val) = At pos $ f val

instance Show a => Show (Located a) where
  show (At _ val) = show val

data Position =
  SrcPos Int Int
  | FromBuiltin
  | Dummy
  deriving (Eq, Show)

toPos :: Located a -> Position
toPos (At pos _) = pos

toValue :: Located a -> a
toValue (At _ v) = v

withDummy :: a -> Located a
withDummy = At Dummy
