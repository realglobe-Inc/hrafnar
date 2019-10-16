module Hrafnar.CoreSpec(spec) where

import           Hrafnar.Annotation
import           Hrafnar.Builtin
import           Hrafnar.Core
import           Hrafnar.Parser
import           Test.Hspec

import           Control.Lens       hiding (Context, List)
import           Control.Monad.RWS
import           Text.Megaparsec

ev :: String -> Value
ev s =
  let
    decls = case parse topLevel "CoreSpec" s of
      Right ds -> ds
      Left e   -> error $ show e
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

    it "partial applying 1" $ ev
      ( "f = add 0\n" <>
        "g = eq 1\n" <>
        "main = f 2"
      )
      `shouldBe`
      VInt 2

    it "partial applying 2" $ ev
       ( "f = add 0\n" <>
         "g = eq 1\n" <>
         "main = g 2"
       )
      `shouldBe`
      VBool False

  describe "data constructor" $ do

    it "nullary" $ ev
       ( "main = T\n" <>
         "data B = T | F"
       )
      `shouldBe`
      VCon "T"[]

    it "unary" $ ev
       ( "main = Cons 0\n" <>
         "data Wrap = Cons Int"
       )
      `shouldBe`
      VCon "Cons" [VInt 0]

    it "binary" $ ev
       ( "main = Cons 0 1\n" <>
         "data Wrap = Cons Int Int"
       )
      `shouldBe` VCon "Cons"[VInt 0, VInt 1]

    it "partial application" $ ev
       ( "main = Cons 0\n" <>
         "data Wrap = Cons Int Int"
       )
      `shouldBe`
      VCon "Cons" [VInt 0]

    it "nested" $ ev
       ( "main = Outer (Inner 0)\n" <>
         "data Outer = Outer Inner\n" <>
         "data Inner = Inner Int"
       )
      `shouldBe`
      VCon "Outer" [VCon "Inner" [VInt 0]]

  describe "case" $ do

    it "variable" $ ev
       ( "main =\n" <>
         "  case x of\n" <>
         "    y -> y\n" <>
         "x = 0"
       )
      `shouldBe`
      VInt 0

    it "first branch" $ ev
       ( "main =\n" <>
         "  case x of\n" <>
         "   1 -> 2\n" <>
         "   2 -> 4\n" <>
         "x = 1"
       )
      `shouldBe`
      VInt 2

    it "second branch" $ ev
       ( "main =\n" <>
         "  case x of\n" <>
         "    1 -> 2\n" <>
         "    2 -> 4\n" <>
         "x = 2"
       )
      `shouldBe`
      VInt 4

    it "deconstruct data type" $ ev
       ( "main =\n" <>
         "  case x of\n" <>
         "    Foo y -> y \n" <>
         "data Hoge = Foo Int\n" <>
         "x = Foo 0"
       )
      `shouldBe`
      VInt 0

    it "nested data type" $ ev
       ( "main =\n" <>
         "  case x of\n" <>
         "     Hoge (Bar 1) -> 1\n" <>
         "     Hoge (Foo 0) -> 0\n" <>
         "\n" <>
         "data Hoge = Hoge FooBar\n" <>
         "\n" <>
         "data FooBar = Foo Int | Bar Int\n" <>
         "\n" <>
         "x = Hoge (Foo 0)"
       )
      `shouldBe`
      VInt 0

    it "constructor that has multi arguments" $ ev
       ( "main =\n" <>
         "  case x of\n" <>
         "    Foo y z -> add y z\n" <>
         "data Foo = Foo Int Int\n" <>
         "x = Foo 1 2"
       )
      `shouldBe`
      VInt 3

    it "constructor that has multi arguments, one of which is data type" $ ev
       ( "main =\n" <>
         "  case x of\n" <>
         "    Hoge (Foo y) z -> add y z\n" <>
         "\n" <>
         "data Hoge = Hoge Foo Int\n" <>
         "\n" <>
         "data Foo = Foo Int\n" <>
         "\n" <>
         "x = Hoge (Foo 1) 2"
       )
      `shouldBe`
      VInt 3


  describe "evalMain" $

    it "declaration order does not matter" $ ev
    ( "main =\n" <>
      "  f 2\n" <>
      "f = add 0"
    )
      `shouldBe`
      VInt 2
