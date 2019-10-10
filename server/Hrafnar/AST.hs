{-|
Description : AST of hrafnar-lang
Module      : Hrafnar.AST
Copyright   : REALGLOBE INC. (c) REALGLOBE 2018
License     : BSD3

Maintainer  : REALGLOBE INC.
-}
{-# LANGUAGE DeriveGeneric #-}

module Hrafnar.AST
  ( OpAssoc(..)
  , Expr'(..)
  , Expr
  , Lit(..)
  , Pat
  , Pat'(..)
  , DataDecl
  , Decl'(..)
  , Decl
  , Line(..)
  , splitDecls
  ) where

import           Hrafnar.Annotation
import           Hrafnar.Assoc
import           Hrafnar.Types

import qualified Data.Aeson         as AE
import           GHC.Generics

-- | Wrapper for value
data Lit
  = Bool Bool
  | Int Int
  | Char Char
  | String String
  | Tuple [Expr]
  | List [Expr]
  deriving (Show, Eq, Generic)

instance AE.ToJSON Lit where
  toJSON = undefined

instance AE.FromJSON Lit where
  parseJSON = undefined

-- | AST of Hrafnar ML
data Expr'
  = Apply Expr Expr
  | Lambda Name Expr
  | Var Name
  | Lit Lit
  | If Expr Expr Expr
  | Let [Decl] Expr
  | Case Expr [(Pat, Expr)]
  | Do [Expr]
  deriving (Eq, Generic)

type Expr = Located Expr'

instance Show Expr' where
  show (Apply x y)  = "(Apply " <> show x <> show y <> ")"
  show (Lambda x e) = "(Lambda " <> x <> " " <> show e <> ")"
  show (Var x)      = "(Var " <> x <> ")"
  show (Lit x)      = "(Lit " <> show x <> ")"
  show (If x y z)   = "(If " <> show x <> " then " <> show y <> " else " <> show z <>")"
  show (Let ds e)   = "(Let " <> show ds <> " in " <> show e <> ")"
  show Case{}       = "case"
  show (Do xs)      = "(Do" <> show xs <> ")"

-- | Patterns.
type Pat = Located Pat'

data Pat'
  = PVar Name
  | PLit Lit
  | PCon Name [Pat]
  | PCons Pat Pat
  | PWildcard
  deriving (Eq, Generic, Show)

-- | Declaration.
type DataDecl = (Name, ([Name], [(Name, Type)]))
data Decl'
  = ExprDecl Name Expr
  | DataDecl Name [Name] [(Name, Type)]
  | TypeAnno [Name] Type
  deriving (Eq, Show)

type Decl = Located Decl'

-- | For REPL.
data Line
  = ExprLine Expr
  | DeclLine Decl
  deriving (Eq, Show)

splitDecls :: [Decl] -> ([(Name, Expr)], [(Name, Type)], [DataDecl]) -> ([(Name, Expr)], [(Name, Type)], [DataDecl])
splitDecls [] ds' = ds'
splitDecls (At _ (ExprDecl name expr):decls) (exprs, typs, dats) = splitDecls decls ((name , expr) : exprs, typs, dats)
splitDecls (At _ (TypeAnno names typ):decls) (exprs, typs, dats) = splitDecls decls (exprs, fmap (, typ) names <> typs, dats)
splitDecls (At _ (DataDecl name tvs cons):decls) (exprs, typs, dats) = splitDecls decls (exprs, typs, (name, (tvs, cons)) : dats)
