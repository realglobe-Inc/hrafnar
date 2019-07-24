module Main where

import           Control.Monad
import           Hrafnar.DI

import qualified SpecHrafnar
import           Test.Hspec
import           Test.Hspec.Formatters
import           Test.Hspec.Runner



main :: IO ()
main =
  hspecWith defaultConfig SpecHrafnar.spec
