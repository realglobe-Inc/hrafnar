module Hrafnar.CoreSpec(spec) where

import           Hrafnar.Annotation
import           Hrafnar.Builtin
import           Hrafnar.Core
import           Hrafnar.Parser
import           Test.Hspec

import           Control.Lens       hiding (Context, List)
import           Control.Monad.RWS
import Text.Megaparsec

ev :: String -> Value
ev s =
  let
    decls = case parse declsParser "CoreSpec" s of
      Right ds -> ds
      Left _   -> error "error"
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

  describe "data constructor" $ do
    it "nullary" $
      ev "main = T; data B = T | F" `shouldBe` VCon "T" []

    it "unary" $
      ev "main = Cons 0; data Wrap = Cons Int" `shouldBe` VCon "Cons" [VInt 0]

    it "binary" $
      ev "main = Cons 0 1; data Wrap = Cons Int Int" `shouldBe` VCon "Cons" [VInt 0, VInt 1]

    it "partial application" $
      ev "main = Cons 0; data Wrap = Cons Int Int" `shouldBe` VCon "Cons" [VInt 0]
    it "nested" $
      ev "main = Outer (Inner 0); data Outer = Outer Inner; data Inner = Inner Int" `shouldBe` VCon "Outer" [VCon "Inner" [VInt 0]]

  describe "case" $ do
    it "variable" $
      ev "main = case x of { y -> y }; x = 0" `shouldBe` VInt 0
    it "first branch" $
      ev "main = case x of { 1 -> 2; 2 -> 4}; x = 1" `shouldBe` VInt 2
    it "second branch" $
      ev "main = case x of { 1 -> 2; 2 -> 4}; x = 2" `shouldBe` VInt 4
    it "deconstruct data type" $
      ev "main = case x of { Foo y -> y }; data Hoge = Foo Int; x = Foo 0" `shouldBe` VInt 0
    it "nested data type" $ do
      let p = "main = case x of { Hoge (Bar 1) -> 1; Hoge (Foo 0) -> 0};"
            <> "data Hoge = Hoge FooBar;"
            <> "data FooBar = Foo Int | Bar Int;"
            <> "x = Hoge (Foo 0)"
      ev p `shouldBe` VInt 0
    it "constructor that has multi arguments" $
      ev "main = case x of { Foo y z -> add y z}; data Foo = Foo Int Int; x = Foo 1 2" `shouldBe` VInt 3
    it "constructor that has multi arguments, one of which is data type" $ do
      let p = "main = case x of { Hoge (Foo y) z -> add y z};"
            <> "data Hoge = Hoge Foo Int;"
            <> "data Foo = Foo Int;"
            <> "x = Hoge (Foo 1) 2"
      ev p `shouldBe` VInt 3


  describe "evalMain" $
    it "declaration order does not matter" $
      ev "main = f 2; f = add 0" `shouldBe` VInt 2
