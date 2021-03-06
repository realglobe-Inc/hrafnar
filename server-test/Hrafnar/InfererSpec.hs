module Hrafnar.InfererSpec(spec) where

import           Hrafnar.Annotation
import           Hrafnar.AST
import           Hrafnar.Exception
import           Hrafnar.Inferer
import           Hrafnar.Parser
import           Hrafnar.Types

import           Test.Hspec

import qualified Data.Map.Strict    as MA
import           Text.Megaparsec

hl :: String -> Expr
hl s =
  let
    decls = case parse topLevel "InferSpec" s of
      Right ds -> ds
      Left e   -> error $ show e
  in
    withDummy . Let decls . withDummy $  Var "main"


spec :: Spec
spec = do

  describe "basics" $ do

    it "literal" $ do
      s <- infer MA.empty $
           hl "main = 1"
      s `shouldBe` Forall [] tyInt

    it "variable" $ do
      s <- infer MA.empty $ hl
           ( "main = x\n" <>
             "x = 1"
           )
      s `shouldBe` Forall [] tyInt

    it "variable" $
      infer MA.empty
      (hl
        ( "main = y\n" <>
          "x = 1"
        )
      )
      `shouldThrow`
      \case
        UnboundVariable "y" _ -> True
        _ -> False


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
      s `shouldBe` Forall [] (TyTuple [TyCon "Int" [], TyCon "Bool" []])

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
      s <- infer MA.empty $ hl
        ( "main : Int\n" <>
          "main = 1"
        )
      s `shouldBe` Forall [] tyInt

    it "unmatch" $
      infer MA.empty (hl
          ( "main : String\n" <>
            "main = 1"
          ))
      `shouldThrow`
      \case
        FailedUnification _ _ -> True
        _ -> False

  describe "let" $ do

    it "with type annotation" $ do
      s <- infer MA.empty $ hl
           ( "main = f\n" <>
             "f : Int\n" <>
             "f = 1"
           )
      s `shouldBe` Forall [] tyInt

    it "without type annotation" $ do
      s <- infer MA.empty $
           hl "main = let f = 1 in f"
      s `shouldBe` Forall [] tyInt

    it "forward declaration with type annotation" $ do
      s <- infer MA.empty $ hl
           ( "main =\n" <>
             "  let\n" <>
             "   f : Int\n" <>
             "   f = g\n" <>
             "   g = h\n" <>
             "   h = 1\n" <>
             "  in f"
           )
      s `shouldBe` Forall [] tyInt

    it "forward declaration without type annotation" $ do
      s <- infer MA.empty $ hl
           ( "main =\n" <>
             "  let\n" <>
             "    f = g\n" <>
             "    g = h\n" <>
             "    h = 1\n" <>
             "  in f"
           )
      s `shouldBe` Forall [] tyInt

    it "simple recursion with type annotation" $ do
      s <- infer MA.empty . hl $
           "main = f 1\n" <>
           "data Bool = True | False\n" <>
           "f : Int -> Int\n" <>
           "f = \\x -> if True then x else f x"
      s `shouldBe` Forall [] tyInt

    it "simple recursion without type annotation" $ do
      s <- infer MA.empty . hl $
           "main = f 1\n" <>
           "data Bool = True | False\n" <>
           "f = \\x -> if True then x else f x"
      s `shouldBe` Forall [] tyInt


  describe "ADT" $ do

    it "basic" $ do
      s <- infer MA.empty . hl $
        "main = True\n" <>
        "data Bool = True | False"
      s `shouldBe` Forall [] tyBool

    it "with values" $ do
      s <- infer MA.empty . hl $
        "main = Foo 1\n" <>
        "data Hoge = Foo Int"
      s `shouldBe` Forall [] (TyCon "Hoge" [])

    it "partial application" $ do
      s <- infer MA.empty . hl $
        "main = Foo 1\n" <>
        "data Hoge = Foo Int Int"
      s `shouldBe` Forall [] (TyFun tyInt $ TyCon "Hoge" [])

    it "type variable with type annotation" $ do
      s <- infer MA.empty . hl $
           "data Maybe a = Just a | Nothing\n" <>
           "main : Maybe Int\n" <>
           "main = Just 1"
      s `shouldBe` Forall [] (TyCon "Maybe" [tyInt])

    it "type variable without type annotation" $ do
      s <- infer MA.empty . hl $
           "data Maybe a = Just a | Nothing\n" <>
           "main = Just 1"
      s `shouldBe` Forall [] (TyCon "Maybe" [tyInt])


  describe "case" $ do

    context "normal cases" $ do

      it "num" $ do
        s <- infer MA.empty . hl $
          ( "main =\n" <>
            "  case 1 of\n" <>
            "    1 -> 1\n" <>
            "    2 -> 2")
        s `shouldBe` Forall [] tyInt

      it "variable" $ do
        s <- infer MA.empty . hl $
          ( "main =\n" <>
            "  case 1 of\n" <>
            "    x -> x\n" <>
            "    2 -> 2"
          )
        s `shouldBe` Forall [] tyInt

      it "variable2" $ do
        s <- infer MA.empty . hl $
          ( "main =\n" <>
            "  \\x ->\n" <>
            "    case x of\n" <>
            "      1 -> 2\n" <>
            "      x -> x")
        s `shouldBe` Forall [] (TyFun tyInt tyInt)

      it "data constructor" $ do
        s <- infer MA.empty . hl $
             "data Hoge = Foo Int\n" <>
             "main =\n" <>
             "  \\x ->\n" <>
             "    case x of\n" <>
             "      Foo y -> y"
        s `shouldBe` Forall [] (TyFun (TyCon "Hoge" []) tyInt)

      it "data constructor with same variable name" $ do
        s <- infer MA.empty . hl $
             "data Hoge = Foo Int\n" <>
             "main =\n" <>
             "  \\x ->\n" <>
             "    case x of\n" <>
             "      Foo x -> x"
        s `shouldBe` Forall [] (TyFun (TyCon "Hoge" []) tyInt)

      it "data constructor with branches" $ do
        s <- infer MA.empty . hl $
             "data Hoge = Foo Int | Bar String\n" <>
             "main =\n" <>
             "  \\x ->\n" <>
             "    case x of\n" <>
             "      Foo y -> y\n" <>
             "      Bar _ -> 2"
        s `shouldBe` Forall [] (TyFun (TyCon "Hoge" []) tyInt)

      it "complex data constructor" $ do
        s <- infer MA.empty . hl $
             "data Hoge = Foo Int String Int\n" <>
             "main =\n" <>
             "  \\x ->\n" <>
             "    case x of\n" <>
             "      Foo x _ 1 -> 1\n" <>
             "      Foo x _ 3 -> x"
        s `shouldBe` Forall [] (TyFun (TyCon "Hoge" []) tyInt)


      it "nested data constructor" $ do
        s <- infer MA.empty . hl $
             "data Hoge = Foo Fuga\n" <>
             "data Fuga = Bar Int\n" <>
             "main =\n" <>
             "  \\x ->\n" <>
             "    case x of\n" <>
             "      Foo (Bar x) -> x"
        s `shouldBe` Forall [] (TyFun (TyCon "Hoge" []) tyInt)


    context "throw exception" $ do

      it "different type branches" $
        infer MA.empty
        (hl
         ( "data Bool = True | False\n" <>
           "main =\n" <>
           "  case 1 of\n" <>
           "    x -> 1\n" <>
           "    2 -> True"
         )
        )
        `shouldThrow`
        \case
          FailedUnification _ _ -> True
          _ -> False


      it "duplicated variables" $

        infer MA.empty
        (hl
         ( "data Hoge = Foo Int Int\n" <>
           "main =\n" <>
           "  case 1 of\n" <>
           "    Foo x x -> x"
         )
        )
        `shouldThrow`
        \case
          DuplicatedPatternVariables _ _ -> True
          _ -> False

      it "not exist data constructor" $
        infer MA.empty
        (hl
         ( "data Spam = Spam\n" <>
           "main =\n" <>
           "  case Spam of\n" <>
           "    Voom -> 1"
         )
        )
        `shouldThrow`
        \case
          DataConstructorNotFound "Voom" _ -> True
          _ -> False

      it "pass wrong arguments to data constructor" $
        infer MA.empty
        (hl
         ( "data Hoge = Foo Int\n" <>
           "data Bool = True | False\n" <>
           "main = \n" <>
           "  case Foo 1 of\n" <>
           "    Foo True -> 1"
         )
        )
        `shouldThrow`
        \case
          TypeUnmatched _ _ -> True
          _ -> False


    context "polymorphism" $ do

      it "dependencies should be correctly separated" $ do
        s <- infer (MA.singleton "add" (Forall [] $ TyFun tyInt (TyFun tyInt tyInt))) $ hl
             ( "main = y\n" <>
               "id = \\x -> x\n" <>
               "x = id 0\n" <>
               "y = (id (\\z -> add x z)) 1"
             )
        s `shouldBe` Forall [] tyInt

      it "type annotation" $ do
        s <- infer MA.empty $ hl
             ( "main : Int -> Int\n" <>
               "main = id\n" <>
               "id = \\x -> x"
             )
        s `shouldBe` Forall [] (TyFun tyInt tyInt)

      it "occurs check" $
        infer MA.empty
        (hl
         ( "f = \\g x -> g g x\n" <>
           "main = 1"
         )
        )
        `shouldThrow`
        \case
          OccursCheckFailed _ _ -> True
          _ -> False
