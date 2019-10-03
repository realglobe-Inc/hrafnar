module Hrafnar.ParserSpec(spec) where

import           Hrafnar.Annotation
import           Hrafnar.AST
import           Hrafnar.Builtin
import           Hrafnar.Parser
import           Hrafnar.Types

import           Test.Hspec

import           Data.Void
import           Text.Megaparsec

spec :: Spec
spec = do
  describe "expressions" $ do

    context "if" $ do

      it "parse if" $

        parseExpr "if x then y else z"
        `shouldBe`
        Right (If' (Var' "x") (Var' "y") (Var' "z"))

      it "if has right assoc" $
        parseExpr "if x then y else f z"
        `shouldBe`
        Right (If' (Var' "x") (Var' "y") (Apply' (Var' "f") (Var' "z")))

      it "with comments" $
        parseExpr "if{-spam-}x{-spam-} then{-spam-} y {-spam-}else{-spam-} z -- spam"
        `shouldBe`
        Right (If' (Var' "x") (Var' "y") (Var' "z"))


    context "case" $ do

      it "simple" $
        parseExpr
        ( "case x of\n" <>
          "  x -> 1"
        )
        `shouldBe`
        Right (Case' (Var' "x") [(PVar' "x", Lit' $ Int' 1)])

      it "indented branch" $
        parseExpr
        ( "case x of\n" <>
          "  x ->\n" <>
          "    1"
        )
        `shouldBe`
        Right (Case' (Var' "x") [(PVar' "x", Lit' $ Int' 1)])

      it "wild card" $
        parseExpr
        ( "case x of\n" <>
          "  1 -> 1\n" <>
          "  _ -> 2"
        )
        `shouldBe`
        Right (Case' (Var' "x")
               [ (PLit' $ Int' 1, Lit' $ Int' 1)
               , (PWildcard', Lit' $ Int' 2)
               ])

      it "data constructor" $
        parseExpr
        ( "case x of\n" <>
          "  Foo -> 1\n" <>
          "  Bar x -> 2"
        )
        `shouldBe`
        Right (Case' (Var' "x")
               [ (PCon' "Foo" [], Lit' (Int' 1))
               , (PCon' "Bar" [PVar' "x"], Lit' (Int' 2))])

      it "nested data constructor" $
        parseExpr
        ( "case x of\n" <>
          "  Foo ( Bar (Baz x)) -> 1"
        )
        `shouldBe`
        Right (Case' (Var' "x")
               [ (PCon' "Foo" [PCon' "Bar" [PCon' "Baz" [PVar' "x"]]], Lit' (Int' 1)) ])

{-
  describe "do" $

    it "parse do" $
      fromExpr <$> runAlex "do { 1; 2; 3 }" exprParser
      `shouldBe`
      Right (Do' [Lit' $ Int' 1, Lit' $ Int' 2, Lit' $ Int' 3])

-}
    context "let in" $ do

      it "parse let in" $
        parseExpr
        ( "let\n" <>
          "  x = 1\n" <>
          "  y = 2\n" <>
          "in x"
        )
        `shouldBe`
        Right (Let' [ExprDecl' "x" (Lit' $ Int' 1), ExprDecl' "y" (Lit' $ Int' 2)] (Var' "x"))

      it "with comments" $
        parseExpr
        ( "let--spam\n" <>
          "  x{-spam-}={-spam-}{-spam-} 1 -- spam\n" <>
          "  y {-spam-}={-spam-}2{-spam-}\n" <>
          "in{-spam-}x"
        )
        `shouldBe`
        Right (Let' [ExprDecl' "x" (Lit' $ Int' 1), ExprDecl' "y" (Lit' $ Int' 2)] (Var' "x"))

    context "apply" $ do

      it "pass multipul args" $
        parseExpr "f 1 2"
        `shouldBe`
        Right (Apply' (Apply' (Var' "f") (Lit' $ Int' 1)) (Lit' $ Int' 2))

      it "pass paren as first arg" $
        parseExpr "f (g 1) 1"
        `shouldBe`
        Right (Apply'
               (Apply' (Var' "f") (Apply' (Var' "g") (Lit' $ Int' 1)))
               (Lit' $ Int' 1)
            )

      it "pass paren as second arg" $
        parseExpr "f 1 (g 1)"
        `shouldBe`
        Right (Apply'
               (Apply' (Var' "f") (Lit' $ Int' 1))
               (Apply' (Var' "g") (Lit' $ Int' 1))
              )

      it "apply an infix operator" $
        parseExpr "1 * 2"
        `shouldBe`
        Right (Apply' (Apply' (Lit' (Int' 1)) (Var' "*")) (Lit' (Int' 2)))

      it "apply infix operators" $
        -- Association of operators is going to be resolved later stage.
        -- All operators are regarded as which have same priority and
        -- left associative, like (((1 * 2) + 3) / 4).
        parseExpr "1 * 2 + 3 / 4"
        `shouldBe`
        Right (Apply' (Apply' (Apply' (Apply' (Apply' (Apply' (Lit' (Int' 1)) (Var' "*")) (Lit' (Int' 2))) (Var' "+")) (Lit' (Int' 3))) (Var' "/")) (Lit' (Int' 4)))

    context "lambda" $ do

      it "multi args lambda" $
        parseExpr "\\x y -> x"
        `shouldBe`
        Right (Lambda' "x" (Lambda' "y" (Var' "x")))

      it "lambda with comments" $
        parseExpr "\\{-spam-}x {-spam-}y {-spam-}-> {-spam-}x -- spam"
        `shouldBe`
        Right (Lambda' "x" (Lambda' "y" (Var' "x")))


  describe "declarations" $ do

    context "expression" $ do
      it "expression delaration" $
        parseDecl "x = 1"
        `shouldBe`
        Right
        (ExprDecl' "x" (Lit' $ Int' 1))

      it "expression delaration with comments" $
        parseDecl "x{- spam -} = {- spam -} 1 -- spam"
        `shouldBe`
        Right
        (ExprDecl' "x" (Lit' $ Int' 1))


    context "type annotation" $ do

      it "single" $
        parseDecl "foo : Int"
        `shouldBe`
        Right (TypeAnno' ["foo"] tyInt)


      it "multi" $
        parseDecl  "foo, bar, baz : String"
        `shouldBe`
        Right (TypeAnno' ["foo", "bar", "baz"] tyString)

      it "function type" $
        parseDecl "foo : Int -> String -> Bool"
        `shouldBe`
        Right (TypeAnno' ["foo"] (TyFun (TyCon "Int") (TyFun (TyCon "String") (TyCon "Bool"))))

      it "type variables" $
        parseDecl "foo : a -> b"
        `shouldBe`
        Right (TypeAnno' ["foo"] (TyFun (TyVar $ TV "a") (TyVar $ TV "b")))

      it "ADT" $
        pendingWith "need to add type application"
{-
        parseDecl "foo : Maybe a"
        `shouldBe`
        Right (TypeAnno' ["foo"] (TyCon "Maybe"))
-}
      it "parens" $
        parseDecl "foo : (String -> Bool) -> Int"
        `shouldBe`
        Right (TypeAnno' ["foo"] (TyFun (TyFun tyString tyBool) tyInt))

    context "data constructor" $ do

      it "basic" $
        parseDecl "data Hoge = Foo | Bar"
        `shouldBe`
        Right (DataDecl' "Hoge" [] [("Foo", TyCon "Hoge"), ("Bar", TyCon "Hoge")])

      it "with values" $
        parseDecl "data Hoge = Foo Int String"
        `shouldBe`
        Right (DataDecl' "Hoge" [] [("Foo", TyFun tyInt (TyFun tyString (TyCon "Hoge")))])

      it "with type variables" $
        parseDecl "data Hoge a = Foo a"
        `shouldBe`
        Right (DataDecl' "Hoge" ["a"] [("Foo", TyFun (TyVar $ TV "a") (TyCon "Hoge"))])


    describe "top-level declarations" $

      it "simple declaration" $
        parseTopLevel
        ( "main : String\n" <>
          "main = 1"
        )
        `shouldBe`
        Right
        [ TypeAnno' ["main"] (TyCon "String")
        , ExprDecl' "main" (Lit' $ Int' 1)
        ]

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

parseExpr :: String -> Either (ParseErrorBundle String Void) ExprSrc
parseExpr x = fromExpr <$> parse (exprParser <* eof) "ParserSpec" x

parseDecl :: String -> Either (ParseErrorBundle String Void) DeclSrc
parseDecl x = fromDecl <$> parse (declParser <* eof) "ParserSpec" x

parseTopLevel :: String -> Either (ParseErrorBundle String Void) [DeclSrc]
parseTopLevel x = fmap fromDecl <$> parse topLevel "ParserSpec" x

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

    fromLit (Bool b)      = Lit' . Bool' $ b
    fromLit (Int i)       = Lit' . Int' $ i
    fromLit (Tuple exprs) = Lit' . Tuple' $ fmap fromExpr exprs

    unLit (Lit' l) = l

fromDecl :: Decl -> DeclSrc
fromDecl (At _ decl) = fromDecl' decl
  where
    fromDecl' (ExprDecl n expr)  = ExprDecl' n $ fromExpr expr
    fromDecl' (DataDecl n ns cs) = DataDecl' n ns cs
    fromDecl' (TypeAnno ns typ)  = TypeAnno' ns typ
