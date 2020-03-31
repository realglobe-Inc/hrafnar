module Main where

import qualified SpecUtil
import           Test.Hspec.Runner

main :: IO ()
main =
  hspecWith defaultConfig SpecUtil.spec
