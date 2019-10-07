{-|
Description : Assoc of ops
Module      : Hrafnar.Assoc
Copyright   : REALGLOBE INC. (c) REALGLOBE 2018
License     : BSD3

Maintainer  : REALGLOBE INC.
-}
module Hrafnar.Assoc(OpAssoc(..)) where

-- | Assosiative of the operator.
data OpAssoc
  = OpL
  | OpR
  deriving (Show, Eq)
