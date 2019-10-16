{-|
Description : Exceptions.
Module      : Hrafnar.Exception
Copyright   : REALGLOBE INC. (c) REALGLOBE 2018
License     : BSD3

Maintainer  : REALGLOBE INC.
-}
module Hrafnar.Exception
  ( EvalException(..)
  , InferenceException(..)
  , RecipeException(..)
  , ParserException(..)
  , maxEvalLimit
  , evalWithLimitCheck
  , SomeException
  ) where

import           Hrafnar.Annotation
import           Hrafnar.Types

import           Control.Exception.Safe
import           Data.List              (intercalate)
import           Text.Megaparsec.Error

-- | Exception for evaluator.
data EvalException
  = UnableApplyingToValue
  | VariableNotDeclared String
  | OrphanFunction
  | EmptyAnonymousFunction
  | DifferentTypesInList
  | FailCompileLit
  deriving (Show)

instance Exception EvalException

-- | Exception for inferer.
data InferenceException
  = TypeVariableNotFound Name
  | TypeVariableExhausted
  | InvalidConstraints
  | UnboundVariable Name Position
  | DataConstructorNotFound Name Position
  | DuplicatedPatternVariables Name Position
  | TypeUnmatched Type Type
  | CantApplyToValue Type Type
  | FailedUnification Type Type
  | FailedUnifications [Type] [Type]
  | OccursCheckFailed Name Type
  | EmptyDo
  deriving (Show, Eq)

instance Exception InferenceException

-- | Exception for parser.
data ParserException
  = ReservedKeyWord
  | ReservedSymbol
  deriving(Show, Ord, Eq)

instance Exception ParserException

instance ShowErrorComponent ParserException where
  showErrorComponent = show


data RecipeException
  = forall a. Show a => TypeError String [a]
  | forall a. Show a => KeyError String a
  | MaxEvalLimitExceeded

instance Show RecipeException where
  show (TypeError msg args) = "TypeError: " <> msg <> "Passed args: " <> intercalate ", " (fmap show args)
  show (KeyError msg key) = "KeyError: " <> msg <> "Passed key: " <> show key
  show MaxEvalLimitExceeded = "MaxEvalLimitExceeded: limit is " <> show maxEvalLimit

instance Exception RecipeException

-- | magic-number
maxEvalLimit :: Int
maxEvalLimit = 2048

evalWithLimitCheck :: (Monad m, MonadThrow m) => Int -> m a -> m a
evalWithLimitCheck count evalAction = if count < maxEvalLimit
    then evalAction
    else throw MaxEvalLimitExceeded
