--{-# LANGUAGE TypeApplications #-}
module Hrafnar.Recipe.CoreSpec(spec) where

import           Hrafnar.DI
import           Hrafnar.Recipe
import           Hrafnar.Recipe.Lexer   hiding (operators)
import           Test.Hspec

import           Control.Exception.Safe
import           Control.Lens           hiding (Context, List)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.RWS
import qualified Data.Aeson             as AE
import           Data.Either            (isLeft, isRight)
import           Data.Extensible
import qualified Data.HashMap.Strict    as HM
import qualified Data.Map.Strict        as MA
import           Data.Scientific
import qualified Data.Text              as ST
import qualified Data.Vector            as VE



ev :: String -> Value
ev s =
  let
    decls = case runAlex s (alexSetUserState (AlexUserState operators) >> parser) of
      Right ds -> ds
      Left e   -> error e
    initSt = initialState & #venv .~ fmap (view _1) builtins
    result = fst <$> evalRWST
             (evalMain decls)
             defaultSituation initSt
  in
    case result of
      Right v -> v
      Left e  -> error $ show e

spec :: Spec
spec = do

  describe "basic operations" $ do
    it "compile Int"  $ do
      let Right result = fst <$> evalRWST
            (compile
              (withDummy $ Lit (Int 1))
            ) defaultSituation initialState
      result `shouldBe` VInt 1

    it "compile do" $ do
      let Right result = fst <$> evalRWST
            (compile
              (withDummy $ Do
                [ withDummy $ Lit (Bool True)
                , withDummy $ Lit (Int 1)
                , withDummy $ Lit (Int 0)
                ]
              )
            ) defaultSituation initialState
      result `shouldBe` VInt 0


  describe "complex operations" $ do
    it "partial applying 1" $
      ev "f = add 0; g = eq 1; main = f 2" `shouldBe` VInt 2

    it "partial applying 2" $
      ev "f = add 0; g = eq 1; main = g 2" `shouldBe` VBool False


  describe "evalMain" $
    it "declaration order does not matter" $
      ev "main = f 2; f = add 0" `shouldBe` VInt 2
