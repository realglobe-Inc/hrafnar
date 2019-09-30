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
  , extractExprs
  , extractTypes
  , extractData
  , binop
  , assoc
  , mkApp
  , mkLambda
  , shrinkTypes
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

extractExprs :: [Decl] -> [(Name, Expr)]
extractExprs []                = []
extractExprs (At _ (ExprDecl n e):ds) = (n, e) : extractExprs ds
extractExprs (_:ds)            = extractExprs ds

extractTypes :: [Decl] -> [(Name, Type)]
extractTypes []                 = []
extractTypes (At _ (TypeAnno ns t):ds) = fmap (, t) ns <> extractTypes ds
extractTypes (_:ds)             = extractTypes ds

extractData :: [Decl] -> [DataDecl]
extractData []                    = []
extractData (At _ (DataDecl n as vs):ds) = (n, (as, vs)) : extractData ds
extractData (_:ds)                = extractData ds

binop :: Expr -> ((String, Int, OpAssoc), Position) -> Expr -> Expr
binop ex1 ((sy, _, _), opPos) ex2 = withDummy
  $ Apply (withDummy $ Apply (At opPos $ Var sy) ex1) ex2

assoc :: Expr -> ((String, Int, OpAssoc), Position) -> Expr -> ((String, Int, OpAssoc), Position) -> Expr -> Expr
assoc ex1 op1@((_, pr1, as1), _) ex2 op2@((_, pr2, as2), _) ex3
 | pr1 > pr2 = binop (binop ex1 op1 ex2) op2 ex3
 | pr1 < pr2 = binop ex1 op1 (binop ex2 op2 ex3)
 | as1 == OpL && as2 == OpL = binop (binop ex1 op1 ex2) op2 ex3
 | as1 == OpR && as2 == OpR = binop ex1 op1 (binop ex2 op2 ex3)
assoc _ _ _ _ _ = undefined

mkApp :: Expr -> [Expr] -> Expr
mkApp = foldl mk
  where
    mk f'@(At pos _) arg = At pos $ Apply f' arg

mkLambda :: Expr -> [Expr] -> Expr
mkLambda = foldr mk
  where
    mk (At pos (Var n)) abst = At pos $ Lambda n abst
    mk _ _                   = undefined

shrinkTypes :: Name -> [(Name, [Type])] -> [(Name, Type)]
shrinkTypes n = fmap (\(n', ts) -> (n', foldr TyFun (TyCon n) ts))
