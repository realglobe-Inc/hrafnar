{-|
Description : Parser of hrafnar-lang
Module      : Hrafnar.Parser
Copyright   : REALGLOBE INC. (c) REALGLOBE 2018
License     : BSD3

Maintainer  : REALGLOBE INC.
-}

module Hrafnar.Parser
  ( exprParser
  , declsParser
  , lineParser
  ) where

import Hrafnar.AST
import Hrafnar.Annotation

import           Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lx
import Data.Void
import qualified Data.List as L
import qualified Data.Set as SE
import Data.Functor
import Control.Monad

type Parser = Parsec Void String

-- | Reserved words.
if_, then_, else_, let_, in_ :: String
if_ = "if"
then_ = "then"
else_ = "else"
let_ = "let"
in_ = "in"

-- | List of reserved words.
reserved :: [String]
reserved =
  [ if_
  , then_
  , else_
  , let_
  , in_
  ]

ifExpr :: Parser Expr
ifExpr = do
  pos <- getSourcePos
  e <- If <$> (string if_ *> expr) <*> (string then_ *> expr) <*> (string else_ *> expr)
  pure $ At (SrcPos pos) e


var :: Parser Expr
var = do
  sym <- some (alphaNumChar <|> symbolChar)
  when (sym `elem` reserved) (failure Nothing SE.empty)
  pos <- getSourcePos
  pure $ At (SrcPos pos) (Var sym)

lambda :: Parser Expr
lambda = do
  pos <- getSourcePos
  e <- Lambda <$> (char '\\' >> space *> some letterChar) <*> (space *> string "->" *> space *> expr)
  pure $ At (SrcPos pos) e

term :: Parser Expr
term =  var <|> lambda <|> parens expr

apply :: Parser Expr
apply = do
  t <- term
  loop t
  where
    loop e = try (loop' e) <|> pure e
    loop' lhs = do
      pos <- getSourcePos
      op <- lexeme $ pure (\e1 e2 -> At (SrcPos pos) (Apply e1 e2))
      rhs <- term
      loop $ op lhs rhs
spaces :: Parser String
spaces = many $ char ' '


expr :: Parser Expr
expr = between spaces spaces $ ifExpr <|> apply <|> term

parens :: Parser Expr -> Parser Expr
parens = between (char '(' <* space) (space *> char ')')


lineComment :: Parser ()
lineComment = Lx.skipLineComment "--"

scn :: Parser ()
scn = Lx.space space1 lineComment empty

sc :: Parser ()
sc = Lx.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = Lx.lexeme sc

exprParser :: Parser Expr
exprParser = expr

declsParser :: Parser [Decl]
declsParser = undefined

lineParser :: Parser Line
lineParser = undefined
