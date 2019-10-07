module Hrafnar.BuiltinSpec(spec) where

import           Hrafnar
import           Hrafnar.Builtin
import           Hrafnar.Core
import           Hrafnar.Value

import           Test.Hspec

import           Control.Lens    hiding (Context, List)
import           Control.Monad
import qualified Data.Map        as MA


injectTest :: (UseDI => a) -> IO a
injectTest io = do
  lq <- newLogQueue
  sysEnv <- getEnv
  injectResources $
    give lq $
    give sysEnv io

spec :: Spec
spec = do
  let Just eq_ = view _1 <$> MA.lookup "eq" builtins
      Just add_ = view _1 <$> MA.lookup "add" builtins
      Just exec_ = view _1 <$> MA.lookup "exec" builtins

  describe "basic operation" $ do
    it "eq"  $ do
      let result = eq_ ! VInt 1 ! VInt 2
      result `shouldBe` VBool False

    it "add"  $ do
      let result = add_ ! VInt 1 ! VInt 2
      result `shouldBe` VInt 3

    it "exec"  $ join $ injectTest $ do
      pending
      let VEff eff = exec_ ! VString "echo nyaan" ! VString ""
      newEvent <- runEffect eff
      newEvent `shouldBe` #local # #exec # "nyaan\n"

