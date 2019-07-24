{-|
Description : Recipe
Module      : Hrafnar.Recipe
Copyright   : REALGLOBE INC. (c) REALGLOBE 2018
License     : BSD3

Maintainer  : REALGLOBE INC.
-}
module Hrafnar.Recipe
  ( module Hrafnar.Recipe.Annotation
  , module Hrafnar.Recipe.Core
  , module Hrafnar.Recipe.Lexer
  , module Hrafnar.Recipe.Parser
  , module Hrafnar.Recipe.Builtin
  , module Hrafnar.Recipe.Event
  , module Hrafnar.Recipe.Exception
  , module Hrafnar.Recipe.Inferer
  , module Hrafnar.Recipe.Types
  , parse
  , defaultSituation
  , initSituation
  ) where

import           Hrafnar.Recipe.Annotation
import           Hrafnar.Recipe.Builtin
import           Hrafnar.Recipe.Core
import           Hrafnar.Recipe.Event
import           Hrafnar.Recipe.Exception
import           Hrafnar.Recipe.Inferer
import           Hrafnar.Recipe.Lexer
import           Hrafnar.Recipe.Parser
import           Hrafnar.Recipe.Types

import           Control.Exception.Safe
import           Control.Lens
import           Data.Extensible
import qualified Data.Map.Strict           as MA

parse :: MonadThrow m => String -> m [Decl]
parse s = case runAlex s $ alexSetUserState (AlexUserState operators) >> parser of
  Right decls -> pure decls
  Left e      -> throwString e

defaultSituation :: Situation
defaultSituation = initSituation MA.empty (#init # ())

initSituation :: ExprTable -> Event -> Situation
initSituation c e =
  #event @= e <:
  #context @= c <:
  #implant @= MA.empty <:
  #effects @= MA.empty <:
  nil
