module Hrafnar.DI.ResourcesSpec where

import           Hrafnar.DI.Resources
import           Hrafnar.Recipe

import           Control.Concurrent.Async (async)
import           Control.Concurrent.STM
import           Control.Monad            (join)
import qualified Data.Aeson               as AE
import qualified Data.ByteString.Lazy     as LBS
import           Data.Extensible
import qualified Data.Map                 as MA
import           Data.ULID
import           Test.Hspec

spec :: Spec
spec =
  describe "manageRecipe" $ do
    it "add recipe" $ do
      pending
      l <- join $ injectResources $ do
        addSampleRecipe
        manageRecipe $ \s -> async $ pure ()
        manager <- readTVarIO recipeManager
        pure . length . MA.keys $ manager
      l `shouldBe` 1
    it "remove recipe" $ do
      pending
      l <- join $ injectResources $ do
        addSampleRecipe
        manageRecipe $ \s -> async $ pure ()
        manager <- readTVarIO recipeManager
        atomically $ writeTQueue recipeQueue (RecipeRemove $ head . MA.keys $ manager)
        manageRecipe $ \s -> async $ pure ()
        length . MA.keys <$> readTVarIO recipeManager
      l `shouldBe` 0


addSampleRecipe :: UseResources => IO ()
addSampleRecipe = undefined
--atomically $ writeTQueue recipeQueue (RecipeAdd . LBS.toStrict . AE.encode $ sampleRecipe)

sampleRecipe :: Recipe
sampleRecipe =
  #initial @= MA.fromList
 -- [ ("counter", Number 0)
--  , ("switch", Bool True)
  [
  ] <:
  #cookbook @= [("main", sampleCookbook)]
  <: nil

sampleCookbook :: Expr
sampleCookbook = withDummy $ Lit $ Bool True --Tuple (Cons (Var "context") (Cons (List []) Nil))
