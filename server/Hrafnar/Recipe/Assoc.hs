module Hrafnar.Recipe.Assoc(OpAssoc(..)) where

-- | Assosiative of the operator.
data OpAssoc
  = OpL
  | OpR
  deriving (Show, Eq)
