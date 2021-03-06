{-|
Description : Types
Module      : Hrafnar.Types
Copyright   : REALGLOBE INC. (c) REALGLOBE 2018
License     : BSD3

Maintainer  : REALGLOBE INC.
-}
module Hrafnar.Types
  ( Name
  , Type(..)
  , TV(..)
  , tyInt
  , tyBool
  , tyChar
  , tyString
  , Scheme(..)
  ) where

type Name = String

newtype TV = TV { unTV :: Name } deriving(Show, Eq, Ord)

-- | Types used by HML
data Type
  = TyVar TV
  | TyCon Name [Type]
  | TyFun Type Type
  | TyTuple [Type]
  | TyList Type
  deriving (Show, Eq, Ord)

tyInt, tyBool, tyChar, tyString :: Type
tyInt = TyCon "Int" []
tyBool = TyCon "Bool" []
tyChar = TyCon "Char" []
tyString = TyCon "String" []

data Scheme = Forall [TV] Type deriving(Show, Eq, Ord)

