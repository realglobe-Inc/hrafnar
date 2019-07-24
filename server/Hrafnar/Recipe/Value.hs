{-|
Description : Definition of Value
Module      : Hrafnar.Recipe.Value
Copyright   : REALGLOBE INC. (c) REALGLOBE 2018
License     : BSD3

Maintainer  : REALGLOBE INC.
-}
{-# LANGUAGE UndecidableInstances #-}
module Hrafnar.Recipe.Value
  ( Value(..)
  , VPat (..)
  , Effect (..)
  ) where

import           Hrafnar.DI
import           Hrafnar.Recipe.Event
import           Hrafnar.Recipe.Types

import           Control.Applicative
import           System.IO.Unsafe     (unsafePerformIO)


-- | Value after compiling @Exp@
data Value
  = VBool Bool
  | VInt Int
  | VChar Char
  | VString String
  | VTuple [Value]
  | VList [Value]
  | VVar String
  | VCon Name
  | VApp Value Value
  | VLam (Value -> Value)
  | VEff (Effect Event)
  | VCase [(VPat, Value)]

data VPat
  = VPBool
  | VPInt Int
  | VPChar Char
  | VPString String
  | VPTuple [Value]
  | VPList [Value]
  | VPVar String
  | VPCon Name


instance Eq Value where
  VBool b == VBool b' = b == b'
  VInt i == VInt i' = i == i'
  VChar c == VChar c' = c == c'
  VString s == VString s' = s == s'
  VTuple vs == VTuple vs' = vs == vs'
  VVar n == VVar n' = n == n'
  VApp f arg == VApp _ arg' = f == f && arg == arg'
  _ == _ = False

instance Show Value where
  show (VBool b)   = show b
  show (VInt i)=   show i
  show (VChar c)=  show c
  show (VString s) = s
  show (VTuple vs) = show vs
  show (VList ls)  = show ls
  show (VVar n)    = "variable " <> show n
  show (VCon n)    = "data constructor " <> n
  show VApp{}      = "<<application>>"
  show VLam{}      = "<<function>>"
  show VEff{}      = "<<effect>>"

-- | Wrapper of @IO@ @Event@ under @UseDI@ constraint.
newtype Effect a = Effect { runEffect :: UseDI => IO a }

instance UseDI => Functor Effect where
  fmap f (Effect io) = Effect $ fmap f io

instance UseDI => Applicative Effect where
  pure = Effect . pure
  liftA2 f (Effect x) (Effect y) = Effect (liftA2 f x y)

instance UseDI => Monad Effect where
  {-# NOINLINE (>>=) #-}
  (Effect x) >>= k = unsafePerformIO $ fmap k x
