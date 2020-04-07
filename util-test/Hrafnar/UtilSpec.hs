module Hrafnar.UtilSpec where

import           Hrafnar.Util

import           Test.Hspec
import           Language.Haskell.TH.TestUtils

import           Language.Haskell.TH.Quote

spec :: Spec
spec = do

  describe "rawC" $ do

    it "behave like single quotes" $

      [rawC|a|] `shouldBe` 'a'

    it "interpret a single quote without any escaping" $

      [rawC|'|] `shouldBe` '\''

    it "interpret a back-slash without any escaping" $

      [rawC|\|] `shouldBe` '\\'

    it "fail when no character is given" $

      $(tryQErr $ quoteExp rawC "")
      `shouldBe`
      Just "no character"

    it "fail when multiple characters are given" $

      $(tryQErr $ quoteExp rawC "ab")
      `shouldBe`
      Just "multiple characters"

  describe "rawS" $ do

    it "behave like double quotes" $

      [rawS|abc|] `shouldBe` "abc"

    it "interpret a double quote without any escaping" $

      [rawS|"|] `shouldBe` "\""

    it "interpret a back-slash without any escaping" $

      [rawS|\|] `shouldBe` "\\"
