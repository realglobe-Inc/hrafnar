module Hrafnar.InfererSpec(spec) where

import           Hrafnar.Annotation
import           Hrafnar.AST
import           Hrafnar.Inferer
import           Hrafnar.Parser
import           Hrafnar.Types

import           Test.Hspec

import qualified Data.Map.Strict    as MA
import Text.Megaparsec

hl :: String -> Expr
hl s =
  let
    decls = case parse declsParser "InferSpec" s of
      Right ds -> ds
      Left _-> error "something wrong"
  in
    withDummy . Let decls . withDummy $  Var "main"


spec :: Spec
spec = do

  describe "collection types" $ do

    it "Tuple" $ do
      s <-
        infer MA.empty
        (withDummy $
          Lit
          (Tuple
            [ withDummy $ Lit (Int 10)
            , withDummy $ Lit (Bool True)
            ]
          )
        )
      s `shouldBe` Forall [] (TyTuple [TyCon "Int",TyCon "Bool"])

    it "List" pending


  describe "function types" $ do

    it "Lambda" $ do
      s <- infer MA.empty $
           hl "main = \\x -> x"
      s `shouldBe` Forall [TV "0"] (TyFun (TyVar $ TV "0") (TyVar $ TV "0"))

    it "Apply" $ do
      let e = MA.fromList [("f", Forall [] (TyFun tyInt tyInt))]
      s <- infer e $
        hl "main = f 1"
      s `shouldBe` Forall [] tyInt

  describe "type annotation" $ do

    it "match" $ do
      s <- infer MA.empty $
           hl "main : Int; main = 1"
      s `shouldBe` Forall [] tyInt

    it "unmatch" $
      infer MA.empty (
          hl "main : String; main = 1"
      ) `shouldThrow` anyException


  describe "let" $ do

    it "with type annotation" $ do
      s <- infer MA.empty $
           hl "main = let f : Int; f = 1 in f"
      s `shouldBe` Forall [] tyInt

    it "without type annotation" $ do
      s <- infer MA.empty $
           hl "main = let f = 1 in f"
      s `shouldBe` Forall [] tyInt

    it "forward declaration with type annotation" $ do
      s <- infer MA.empty $
           hl "main = let f : Int; f = g; g = h; h = 1 in f"
      s `shouldBe` Forall [] tyInt

    it "forward declaration without type annotation" $ do
      s <- infer MA.empty $
           hl "main = let f = g; g = h; h = 1 in f"
      s `shouldBe` Forall [] tyInt

    it "simple recursion with type annotation" $ do
      s <- infer MA.empty . hl $
           "main = let data Bool = True | False;" <>
           "f : Int -> Int; f = \\x -> if True then x else f x in f 1"
      s `shouldBe` Forall [] tyInt

    it "simple recursion without type annotation" $ do
      s <- infer MA.empty . hl $
           "main = let data Bool = True | False;" <>
           "f = \\x -> if True then x else f x in f 1"
      s `shouldBe` Forall [] tyInt


  describe "ADT" $ do

    it "basic" $ do
      s <- infer MA.empty $
           hl "main = let data Bool = True | False in True"
      s `shouldBe` Forall [] tyBool

    it "with values" $ do
      s <- infer MA.empty $
           hl "main = let data Hoge = Foo Int in Foo 1"
      s `shouldBe` Forall [] (TyCon "Hoge")

    it "partial application" $ do
      s <- infer MA.empty $
           hl "main = let data Hoge = Foo Int Int in Foo 1"
      s `shouldBe` Forall [] (TyFun tyInt $ TyCon "Hoge")


  describe "case" $ do

    describe "normal cases" $ do

      it "num" $ do
        s <- infer MA.empty $
             hl "main = case 1 of { 1 -> 1; 2 -> 2 }"
        s `shouldBe` Forall [] tyInt

      it "variable" $ do
        s <- infer MA.empty $
             hl "main = case 1 of { x -> x; 2 -> 2 }"
        s `shouldBe` Forall [] tyInt

      it "variable2" $ do
        s <- infer MA.empty $
             hl "main = \\x -> case x of { 1 -> 2; x -> x }"
        s `shouldBe` Forall [] (TyFun tyInt tyInt)

      it "data constructor" $ do
        s <- infer MA.empty . hl $
             "main = let data Hoge = Foo Int in " <>
             "\\x -> case x of { Foo y -> y }"
        s `shouldBe` Forall [] (TyFun (TyCon "Hoge") tyInt)

      it "data constructor with same variable name" $ do
        s <- infer MA.empty . hl $
             "main = let data Hoge = Foo Int in " <>
             "\\x -> case x of { Foo x -> x }"
        s `shouldBe` Forall [] (TyFun (TyCon "Hoge") tyInt)

      it "data constructor with branches" $ do
        s <- infer MA.empty . hl $
             "main = let data Hoge = Foo Int | Bar String in " <>
             "\\x -> case x of { Foo y -> y; Bar _ -> 2 }"
        s `shouldBe` Forall [] (TyFun (TyCon "Hoge") tyInt)

      it "complex data constructor" $ do
        s <- infer MA.empty . hl $
             "main = let data Hoge = Foo Int String Int in " <>
             "\\x -> case x of { Foo x _ 1 -> 1; Foo x _ 3 -> x }"
        s `shouldBe` Forall [] (TyFun (TyCon "Hoge") tyInt)

      it "nested data constructor" $ do
        s <- infer MA.empty . hl $
             "main = let data Hoge = Foo Fuga; data Fuga = Bar Int in " <>
             "\\x -> case x of { Foo (Bar x) -> x }"
        s `shouldBe` Forall [] (TyFun (TyCon "Hoge") tyInt)


    describe "throw exception" $ do

      it "different type branches" $
        infer MA.empty
        (hl "main = let data Bool = True | False in case 1 of { x -> 1; 2 -> True }")
        `shouldThrow` anyException

      it "duplicated variables" $
        infer MA.empty
        (hl "main = let data Hoge = Foo Int Int in case 1 of { Foo x x -> x }")
        `shouldThrow` anyException

    describe "polymorphism" $
      it "dependencies should be correctly separated" $ do
        s <- infer (MA.singleton "add" (Forall [] $ TyFun tyInt (TyFun tyInt tyInt)))
            (hl "main = y;id = \\x -> x; x = id 0;y = (id (\\z -> add x z)) 1;")
        s `shouldBe` Forall [] tyInt