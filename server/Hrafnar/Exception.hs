module Hrafnar.Exception
  ( EvalException(..)
  , InferenceException(..)
  , RecipeException(..)
  , maxEvalLimit
  , evalWithLimitCheck
  , SomeException
  ) where


import           Control.Exception.Safe
import           Data.List              (intercalate)

data EvalException
  = UnableApplyingToValue
  | VariableNotDeclared String
  | OrphanFunction
  | TypeUnmatched
  | EmptyAnonymousFunction
  | DifferentTypesInList
  | FailCompileLit
  deriving (Show)

instance Exception EvalException

data InferenceException
  = TypeVariableNotFound String
  | TypeVariableExhausted
  | InvalidConstraints
  | EmptyDo
  deriving (Show)

instance Exception InferenceException

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
