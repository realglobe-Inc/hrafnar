module Hrafnar.ParserSpec(spec) where

import           Hrafnar.Annotation
import           Hrafnar.AST
import           Hrafnar.Builtin
import           Hrafnar.Parser
import           Hrafnar.Types

import           Test.Hspec

import           Data.Void
import           Text.Megaparsec

parseExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseExpr = parse exprParser "ParserSpec"

spec :: Spec
spec = do
  describe "if" $ do
    it "parse if" $
       fromExpr <$> parseExpr "if x then y else z"
       `shouldBe`
       Right (If' (Var' "x") (Var' "y") (Var' "z"))
    it "if has right assoc" $
      fromExpr <$> parseExpr "if x then y else f z"
      `shouldBe`
      Right (If' (Var' "x") (Var' "y") (Apply' (Var' "f") (Var' "z")))

--  describe "tuple" $ do

    -- it "tuple" $ do
    --   let p = alexSetUserState (AlexUserState MA.empty MA.empty) >> parser
    --   let Right decls = runAlex "(1,2)" p
    --   let Just expr = lookupExpr "main" decls
    --   let Right result = fst <$> evalRWST (compile exp) defaultSituation initialState
    --   result `shouldBe` Tuple [Lit $ Int 1, Lit $ Int 2]
    --
    -- it "empty tuple" $ do
    --   let p = alexSetUserState (AlexUserState MA.empty MA.empty) >> parser
    --   let Right decls = runAlex "()" p
    --   let Just expr = lookupExpr "main" decls
    --   let Right result = fst <$> evalRWST (compile exp) defaultSituation initialState
    --   result `shouldBe` Tuple []

{-

  describe "case" $ do

    it "simple" $
      fromExpr <$> runAlex "case x of { x -> 1 }" exprParser
      `shouldBe`
      Right (Case' (Var' "x") [(PVar' "x", Lit' $ Int' 1)])

    it "wild card" $
      fromExpr <$> runAlex "case x of { 1 -> 1; _ -> 2 }" exprParser
      `shouldBe`
      Right (Case' (Var' "x")
             [ (PLit' $ Int' 1, Lit' $ Int' 1)
             , (PWildcard', Lit' $ Int' 2)
             ])

    it "data constructor" $
      fromExpr <$> runAlex "case x of { Foo -> 1; Bar x -> 2 }" exprParser
      `shouldBe`
      Right (Case' (Var' "x")
             [ (PCon' "Foo" [], Lit' (Int' 1))
             , (PCon' "Bar" [PVar' "x"], Lit' (Int' 2))])

    it "nested data constructor" $
      fromExpr <$> runAlex "case x of { Foo (Bar (Baz x)) -> 1 }" exprParser
      `shouldBe`
      Right (Case' (Var' "x")
             [ (PCon' "Foo" [PCon' "Bar" [PCon' "Baz" [PVar' "x"]]], Lit' (Int' 1)) ])

  describe "do" $

    it "parse do" $
      fromExpr <$> runAlex "do { 1; 2; 3 }" exprParser
      `shouldBe`
      Right (Do' [Lit' $ Int' 1, Lit' $ Int' 2, Lit' $ Int' 3])


  describe "let in" $

    it "parse let in" $
      fromExpr <$> parseExpr
      ( "let\n" <>
        "  x = 1\n" <>
        "  y = 2\n" <>
        "in x + y"
      )
      `shouldBe`
      Right (Let' [ExprDecl' "x" (Lit' $ Int' 1), ExprDecl' "y" (Lit' $ Int' 2)] (Apply' (Apply' (Var' "+") (Var' "x")) (Var' "y")))

-}
  describe "apply" $ do

    it "pass multipul args" $
      fromExpr <$> parseExpr "f 1 2"
      `shouldBe`
      Right (Apply' (Apply' (Var' "f") (Lit' $ Int' 1)) (Lit' $ Int' 2))

    it "pass paren as first arg" $
      fromExpr <$> parseExpr "f (g 1) 1"
      `shouldBe`
      Right (Apply'
             (Apply' (Var' "f") (Apply' (Var' "g") (Lit' $ Int' 1)))
             (Lit' $ Int' 1)
            )

    it "pass paren as second arg" $
      fromExpr <$> parseExpr "f 1 (g 1)"
      `shouldBe`
      Right (Apply'
             (Apply' (Var' "f") (Lit' $ Int' 1))
             (Apply' (Var' "g") (Lit' $ Int' 1))
            )

  describe "lambda" $

    it "multi args lambda" $
      fromExpr <$> parseExpr "\\x y -> x"
      `shouldBe`
      Right (Lambda' "x" (Lambda' "y" (Var' "x")))


{-
  describe "type annotation" $ do

    it "single" $
      runAlex "foo : Int" declParser
      `shouldBe`
      Right (TypeAnno ["foo"] tyInt)


    it "multi" $
      runAlex "foo, bar, baz : String" declParser
      `shouldBe`
      Right (TypeAnno ["foo", "bar", "baz"] tyString)


  describe "data constructor" $ do
    it "basic" $
      runAlex "data Hoge = Foo | Bar" declParser
      `shouldBe`
      Right (DataDecl "Hoge" [] [("Foo", TyCon "Hoge"), ("Bar", TyCon "Hoge")])

    it "with values" $
      runAlex "data Hoge = Foo Int String" declParser
      `shouldBe`
      Right (DataDecl "Hoge" [] [("Foo", TyFun tyInt (TyFun tyString (TyCon "Hoge")))])

    it "with type variables" $
      runAlex "data Hoge a = Foo a" declParser
      `shouldBe`
      Right (DataDecl "Hoge" ["a"] [("Foo", TyFun (TyVar $ TV "a") (TyCon "Hoge"))])
-}

data ExprSrc
  = Apply' ExprSrc ExprSrc
  | Lambda' String ExprSrc
  | Var' String
  | Lit' LitSrc
  | If' ExprSrc ExprSrc ExprSrc
  | Do' [ExprSrc]
  | Case' ExprSrc [(PatSrc, ExprSrc)]
  | Let' [DeclSrc] ExprSrc
  deriving (Show, Eq)

data PatSrc
  = PVar' Name
  | PLit' LitSrc
  | PCon' Name [PatSrc]
  | PCons' PatSrc PatSrc
  | PWildcard'
  deriving (Show, Eq)

data DeclSrc
  = ExprDecl' String ExprSrc
  | DataDecl' Name [Name] [(Name, Type)]
  | TypeAnno' [String] Type
  deriving (Show, Eq)

data LitSrc
  = Bool' Bool
  | Int' Int
  | Tuple' [ExprSrc]
  deriving (Show, Eq)

fromExpr :: Expr -> ExprSrc
fromExpr (At _ expr) = fromExpr' expr
  where
    fromExpr' (Var n)          = Var' n
    fromExpr' (Lit l)=         fromLit l
    fromExpr' (Apply fun args) = Apply' (fromExpr fun) (fromExpr args)
    fromExpr' (Lambda n abst)  = Lambda' n $ fromExpr abst
    fromExpr' (If x y z)       = If' (fromExpr x) (fromExpr y) (fromExpr z)
    fromExpr' (Let ds e)       = Let' (fmap fromDecl ds) (fromExpr e)
    fromExpr' (Case e bs)      =
      Case' (fromExpr e) (fmap (\(p', e') -> (fromPat p', fromExpr e')) bs)
    fromExpr' (Do exprs)       = Do' $ fmap fromExpr exprs

    fromPat (At _ (PVar n))     = PVar' n
    fromPat (At _ (PLit l))     = PLit' . unLit $ fromLit l
    fromPat (At _ (PCon n ps))  = PCon' n $ fmap fromPat ps
    fromPat (At _ (PCons p ps)) = PCons' (fromPat p) (fromPat ps)
    fromPat (At _ PWildcard)    = PWildcard'

    fromDecl (ExprDecl n e)     = ExprDecl' n $ fromExpr e
    fromDecl (DataDecl n as ds) = DataDecl' n as ds
    fromDecl (TypeAnno n t)     = TypeAnno' n t

    fromLit (Bool b)      = Lit' . Bool' $ b
    fromLit (Int i)       = Lit' . Int' $ i
    fromLit (Tuple exprs) = Lit' . Tuple' $ fmap fromExpr exprs

    unLit (Lit' l) = l
