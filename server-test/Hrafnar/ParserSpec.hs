module Hrafnar.ParserSpec(spec) where

import           Hrafnar.Annotation
import           Hrafnar.AST
import           Hrafnar.Builtin
import           Hrafnar.Exception
import           Hrafnar.Parser
import           Hrafnar.Types

import           Test.Hspec
import           Test.Hspec.Megaparsec

import           Data.Either
import           Text.Megaparsec

spec :: Spec
spec = do
  describe "expressions" $ do

    context "literal" $ do

      context "integer" $

        it "parse an integer" $

          parseExpr "42"
          `shouldParse`
          Lit' (Int' 42)

      context "character" $ do

        context "without escaping" $ do

          it "parse a character" $

            parseExpr "'c'"
            `shouldParse`
            Lit' (Char' 'c')

          it "parse a numeric character" $

            parseExpr "'5'"
            `shouldParse`
            Lit' (Char' '5')

          it "parse a space" $

            parseExpr "' '"
            `shouldParse`
            Lit' (Char' ' ')

          it "fail on a backslash" $

            parseExpr "'\\'"
            `shouldFailWith`
            err 3 (ueof <> etok '\'') -- This error means no ending quote mark.

          it "parse a single quote" $

            parseExpr "'''"
            `shouldParse`
            Lit' (Char' '\'')

          it "parse a double quote" $

            parseExpr "'\"'"
            `shouldParse`
            Lit' (Char' '\"')

          it "fail on a zero length character" $

            parseExpr "''"
            `shouldFailWith`
            err 2 (ueof <> etok '\'') -- This error means no ending quote mark.
            -- NOTE: This error depends on whether HML tries to parse a single
            -- quote without escaping or not.
            -- If it did not, the error might be:
            --     err 1 (utok '\'')

          it "fail on a multiple length character" $

            parseExpr "'ab'"
            `shouldFailWith`
            err 2 (utok 'b' <> etok '\'')

        context "with escaping" $ do

          it "parse a special character" $

            parseExpr "'\\n'"
            `shouldParse`
            Lit' (Char' '\n')

          it "parse a decimal unicode" $

            parseExpr "'\\74'"
            `shouldParse`
            Lit' (Char' 'J')

          it "parse a hexadecimal unicode" $

            parseExpr "'\\x4B'"
            `shouldParse`
            Lit' (Char' 'K')

          it "parse a octal unicode" $

            parseExpr "'\\o114'"
            `shouldParse`
            Lit' (Char' 'L')

          it "parse a ascii control code abbreviation" $

            parseExpr "'\\LF'"
            `shouldParse`
            Lit' (Char' '\n')

          it "parse a caret notation" $

            parseExpr "'\\^J'"
            `shouldParse`
            Lit' (Char' '\n')

    context "if" $ do

      it "parse if" $

        parseExpr "if x then y else z"
        `shouldParse`
        If' (Var' "x") (Var' "y") (Var' "z")

      it "if has right assoc" $
        parseExpr "if x then y else f z"
        `shouldParse`
        If' (Var' "x") (Var' "y") (Apply' (Var' "f") (Var' "z"))

      it "with comments" $
        parseExpr "if{-spam-}x{-spam-} then{-spam-} y {-spam-}else{-spam-} z -- spam"
        `shouldParse`
        If' (Var' "x") (Var' "y") (Var' "z")


    context "case" $ do

      it "simple" $
        parseExpr
        ( "case x of\n" <>
          "  x -> 1"
        )
        `shouldParse`
        Case' (Var' "x") [(PVar' "x", Lit' $ Int' 1)]

      it "indented branch" $
        parseExpr
        ( "case x of\n" <>
          "  x ->\n" <>
          "    1"
        )
        `shouldParse`
        Case' (Var' "x") [(PVar' "x", Lit' $ Int' 1)]

      it "wild card" $
        parseExpr
        ( "case x of\n" <>
          "  1 -> 1\n" <>
          "  _ -> 2"
        )
        `shouldParse`
        Case' (Var' "x")
               [ (PLit' $ Int' 1, Lit' $ Int' 1)
               , (PWildcard', Lit' $ Int' 2)
               ]

      it "data constructor" $
        parseExpr
        ( "case x of\n" <>
          "  Foo -> 1\n" <>
          "  Bar x -> 2"
        )
        `shouldParse`
        Case' (Var' "x")
               [ (PCon' "Foo" [], Lit' (Int' 1))
               , (PCon' "Bar" [PVar' "x"], Lit' (Int' 2))]

      it "nested data constructor" $
        parseExpr
        ( "case x of\n" <>
          "  Foo ( Bar (Baz x)) -> 1"
        )
        `shouldParse`
        Case' (Var' "x")
               [ (PCon' "Foo" [PCon' "Bar" [PCon' "Baz" [PVar' "x"]]], Lit' (Int' 1)) ]

{-
  describe "do" $

    it "parse do" $
      fromExpr <$> runAlex "do { 1; 2; 3 }" exprParser
      `shouldParse`
      Do' [Lit' $ Int' 1, Lit' $ Int' 2, Lit' $ Int' 3])

-}
    context "let in" $ do

      it "inline" $
        parseExpr "let x = 1 in x"
        `shouldParse`
        Let' [ExprDecl' "x" (Lit' $ Int' 1)] (Var' "x")

      it "indented" $
        parseExpr
        ( "let\n" <>
          "  x = 1\n" <>
          "  y = 2\n" <>
          "in x"
        )
        `shouldParse`
        Let' [ExprDecl' "x" (Lit' $ Int' 1), ExprDecl' "y" (Lit' $ Int' 2)] (Var' "x")

      it "with comments" $
        parseExpr
        ( "let--spam\n" <>
          "  x{-spam-}={-spam-}{-spam-} 1 -- spam\n" <>
          "  y {-spam-}={-spam-}2{-spam-}\n" <>
          "in{-spam-}x"
        )
        `shouldParse`
        Let' [ExprDecl' "x" (Lit' $ Int' 1), ExprDecl' "y" (Lit' $ Int' 2)] (Var' "x")

    context "apply" $ do

      it "pass multipul args" $
        parseExpr "f 1 2"
        `shouldParse`
        Apply' (Apply' (Var' "f") (Lit' $ Int' 1)) (Lit' $ Int' 2)

      it "pass paren as first arg" $
        parseExpr "f (g 1) 1"
        `shouldParse`
        Apply'
        (Apply' (Var' "f") (Apply' (Var' "g") (Lit' $ Int' 1)))
        (Lit' $ Int' 1)

      it "pass paren as second arg" $
        parseExpr "f 1 (g 1)"
        `shouldParse`
        Apply'
               (Apply' (Var' "f") (Lit' $ Int' 1))
               (Apply' (Var' "g") (Lit' $ Int' 1))

      it "apply an infix operator" $
        parseExpr "1 * 2"
        `shouldParse`
        Apply' (Apply' (Lit' (Int' 1)) (Var' "*")) (Lit' (Int' 2))

      it "apply infix operators" $
        -- Association of operators is going to be resolved later stage.
        -- All operators are regarded as which have same priority and
        -- left associative, like (((1 * 2) + 3) / 4).
        parseExpr "1 * 2 + 3 / 4"
        `shouldParse`
        Apply' (Apply' (Apply' (Apply' (Apply' (Apply' (Lit' (Int' 1)) (Var' "*")) (Lit' (Int' 2))) (Var' "+")) (Lit' (Int' 3))) (Var' "/")) (Lit' (Int' 4))

    context "lambda" $ do

      it "multi args lambda" $
        parseExpr "\\x y -> x"
        `shouldParse`
        Lambda' "x" (Lambda' "y" (Var' "x"))

      it "lambda with comments" $
        parseExpr "\\{-spam-}x {-spam-}y {-spam-}-> {-spam-}x -- spam"
        `shouldParse`
        Lambda' "x" (Lambda' "y" (Var' "x"))

      it "indented" $
        parseExpr
        ( "\\x y ->\n" <>
          "     x"
        )
        `shouldParse`
        Lambda' "x" (Lambda' "y" (Var' "x"))

      it "wrong indented" $
        parseExpr
        ( "\\x y ->\n" <>
          "x"
        )
        `shouldFailWith` errFancy 8 (fancy $ ErrorIndentation GT (mkPos 1) (mkPos 1))

    context "miscellaneous failures" $

      it "reserved keyword" $
        parseExpr "\\if then -> else"
        `shouldFailWith`
        errFancy 3 (fancy $ ErrorCustom ReservedKeyWord)


  describe "declarations" $ do

    context "expression" $ do

      it "expression declaration" $
        parseDecl "x = 1"
        `shouldParse`
        ExprDecl' "x" (Lit' $ Int' 1)

      it "expression declaration with comments" $
        parseDecl "x{- spam -} = {- spam -} 1 -- spam"
        `shouldParse`
        ExprDecl' "x" (Lit' $ Int' 1)

      it "indented" $
        parseDecl
        ( "x =\n" <>
          "  1"
        )
        `shouldParse`
        ExprDecl' "x" (Lit' $ Int' 1)

      it "wrong indented" $
        parseDecl
        ( "x =\n" <>
          "1"
        )
        `shouldFailWith` errFancy 4 (fancy $ ErrorIndentation GT (mkPos 1) (mkPos 1))

    context "type annotation" $ do

      it "single" $
        parseDecl "foo : Int"
        `shouldParse`
        TypeAnno' ["foo"] tyInt

      it "multi" $
        parseDecl  "foo, bar, baz : String"
        `shouldParse`
        TypeAnno' ["foo", "bar", "baz"] tyString

      it "function type" $
        parseDecl "foo : Int -> String -> Bool"
        `shouldParse`
        TypeAnno' ["foo"] (TyFun tyInt (TyFun tyString tyBool))

      it "type variables" $
        parseDecl "foo : a -> b"
        `shouldParse`
        TypeAnno' ["foo"] (TyFun (TyVar $ TV "a") (TyVar $ TV "b"))

      it "ADT" $
        parseDecl "foo : Hoge Int String a"
        `shouldParse`
        TypeAnno' ["foo"] (TyCon "Hoge" [tyInt, tyString, TyVar $ TV "a"])

      it "parens" $
        parseDecl "foo : (String -> Bool) -> Int"
        `shouldParse`
        TypeAnno' ["foo"] (TyFun (TyFun tyString tyBool) tyInt)

    context "data constructor" $ do

      it "basic" $
        parseDecl "data Hoge = Foo | Bar"
        `shouldParse`
        DataDecl' "Hoge" [] [("Foo", TyCon "Hoge" []), ("Bar", TyCon "Hoge" [])]

      it "with types" $
        parseDecl "data Hoge = Foo Int String"
        `shouldParse`
        DataDecl' "Hoge" [] [("Foo", TyFun tyInt (TyFun tyString (TyCon "Hoge" [])))]

      it "with type variables" $
        parseDecl "data Hoge a = Foo a"
        `shouldParse`
        DataDecl' "Hoge" ["a"] [("Foo", TyFun (TyVar $ TV "a") (TyCon "Hoge" [TyVar $ TV "a"]))]

      it "type variables and types" $
        parseDecl "data Hoge a b = Foo a Int b String"
        `shouldParse`
        DataDecl' "Hoge" ["a", "b"]
        [("Foo", TyFun (TyVar $ TV "a") (TyFun tyInt (TyFun (TyVar $ TV "b") (TyFun tyString (TyCon "Hoge" [TyVar $ TV "a", TyVar $ TV "b"])))))]

      it "phantom type" $
        parseDecl "data Proxy a = Proxy"
        `shouldParse`
        DataDecl' "Proxy" ["a"] [("Proxy", TyCon "Proxy" [TyVar $ TV "a"])]


    describe "top-level declarations" $ do

      context "basic" $ do

        it "simple declaration" $
          parseTopLevel
          ( "main : String\n" <>
            "main = 1"
          )
          `shouldParse`
          [ TypeAnno' ["main"] tyString
          , ExprDecl' "main" (Lit' $ Int' 1)
          ]

        it "newline at the end" $
          parseTopLevel
          ( "main : String\n" <>
            "main = 1\n\n"
          )
          `shouldParse`
          [ TypeAnno' ["main"] tyString
          , ExprDecl' "main" (Lit' $ Int' 1)
          ]

        it "newline between declarations" $
          parseTopLevel
          ( "main : String\n" <>
            "\n" <>
            "main = 1"
          )
          `shouldParse`
          [ TypeAnno' ["main"] tyString
          , ExprDecl' "main" (Lit' $ Int' 1)
          ]

        it "multiple" $
          parseTopLevel
          ( "f = 1\n" <>
            "g = f\n"
          )
          `shouldParse`
          [ ExprDecl' "f" (Lit' $ Int' 1)
          , ExprDecl' "g" (Var' "f")
          ]

        it "indented" $
          parseTopLevel
          ( "f =\n" <>
            "  1\n" <>
            "g = f\n"
          )
          `shouldParse`
          [ ExprDecl' "f" (Lit' $ Int' 1)
          , ExprDecl' "g" (Var' "f")
          ]

      context "lambda" $

        it "indented" $
          parseTopLevel
          ( "f =\n" <>
            "  \\x ->\n" <>
            "    1\n" <>
            "g = f\n"
          )
          `shouldParse`
          [ ExprDecl' "f" (Lambda' "x" (Lit' $ Int' 1))
          , ExprDecl' "g" (Var' "f")
          ]

      context "case" $ do

        it "before declaration" $
          parseTopLevel
          ( "f =\n" <>
            "  case Foo of\n" <>
            "    Fuga -> 1\n" <>
            "g = f\n"
          )
          `shouldParse`
          [ ExprDecl' "f" (Case' (Var' "Foo") [(PCon' "Fuga" [], Lit' $ Int' 1)])
          , ExprDecl' "g" (Var' "f")
          ]

        it "after data declaration" $
          parseTopLevel
          ( "data Hoge = Foo | Bar\n" <>
            "f =\n" <>
            "  case Foo of\n" <>
            "    Foo -> 1\n" <>
            "g = f\n"
          )
          `shouldParse`
          [ DataDecl' "Hoge" [] [("Foo", TyCon "Hoge" []), ("Bar", TyCon "Hoge" [])]
          , ExprDecl' "f" (Case' (Var' "Foo") [(PCon' "Foo" [], Lit' $ Int' 1)])
          , ExprDecl' "g" (Var' "f")
          ]

        it "in lambda" $
          parseTopLevel
          ( "data Hoge = Foo | Bar\n" <>
            "f =\n" <>
            "  \\x ->\n" <>
            "    case x of\n" <>
            "      Foo -> 0\n" <>
            "      Bar -> 1\n" <>
            "main = f Foo"
          )
          `shouldParse`
          [ DataDecl' "Hoge" [] [("Foo", TyCon "Hoge" []), ("Bar", TyCon "Hoge" [])]
          , ExprDecl' "f" (Lambda' "x"
                           (Case' (Var' "x")
                            [ (PCon' "Foo" [], Lit' (Int' 0))
                            , (PCon' "Bar" [], Lit' (Int' 1))
                            ]
                           )
                          )
          , ExprDecl' "main" (Apply' (Var' "f") (Var' "Foo"))
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
  | Char' Char
  | Tuple' [ExprSrc]
  deriving (Show, Eq)

parseExpr :: String -> Either (ParseErrorBundle String ParserException) ExprSrc
parseExpr x = fromExpr <$> parse (exprParser <* eof) "ParserSpec" x

parseDecl :: String -> Either (ParseErrorBundle String ParserException) DeclSrc
parseDecl x = fromDecl <$> parse (declParser <* eof) "ParserSpec" x

parseTopLevel :: String -> Either (ParseErrorBundle String ParserException) [DeclSrc]
parseTopLevel x = fmap fromDecl <$> parse topLevel "ParserSpec" x

fromExpr :: Expr -> ExprSrc
fromExpr (At _ expr) = fromExpr' expr
  where
    fromExpr' (Var n)          = Var' n
    fromExpr' (Lit l)          = fromLit l
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
    fromLit (Char c)      = Lit' . Char' $ c
    fromLit (Tuple exprs) = Lit' . Tuple' $ fmap fromExpr exprs

    unLit (Lit' l) = l

fromDecl :: Decl -> DeclSrc
fromDecl (At _ decl) = fromDecl' decl
  where
    fromDecl' (ExprDecl n expr)  = ExprDecl' n $ fromExpr expr
    fromDecl' (DataDecl n ns cs) = DataDecl' n ns cs
    fromDecl' (TypeAnno ns typ)  = TypeAnno' ns typ
