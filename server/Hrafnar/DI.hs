{-|
Module      : Hrafnar.DI
Copyright   : REALGLOBE INC. (c) REALGLOBE 2018
License     : BSD3

Maintainer  : REALGLOBE INC.
-}

module Hrafnar.DI
  ( module Hrafnar.DI.Logger
  , module Hrafnar.DI.Resources
  , module Hrafnar.DI.Conf
  , module Hrafnar.DI.Env
  , UseDI
  ) where

import           Hrafnar.DI.Conf
import           Hrafnar.DI.Env
import           Hrafnar.DI.Logger
import           Hrafnar.DI.Resources

type UseDI =
  ( UseLogger
  , UseResources
  , UseEnv
  )
