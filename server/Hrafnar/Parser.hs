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

import           Hrafnar.Annotation
import           Hrafnar.AST

import           Control.Monad
import           Data.Functor
import qualified Data.List                  as L
import qualified Data.Set                   as SE
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lx

type Parser = Parsec Void String

-- | Reserved words.
if_, then_, else_, let_, in_, (\\), (-->) :: String
if_ = "if"
then_ = "then"
else_ = "else"
let_ = "let"
in_ = "in"
(\\) = "\\"
(-->) = "->"

-- | List of reserved words.
reserved :: [String]
reserved =
  [ if_
  , then_
  , else_
  , let_
  , in_
  ]

-- spaces
sp :: Parser Char
sp = char ' ' <|> char '\''

spaces :: Parser String
spaces = many sp

spaces1 :: Parser String
spaces1 = some sp

trim :: Parser a -> Parser a
trim = between spaces spaces

parens :: Parser Expr -> Parser Expr
parens = between (char '(' <* space) (space *> char ')')

-- signatures
varName :: Parser String
varName = do
  x <- lowerChar
  xs <- many (alphaNumChar <|> char '_' <|> char '\'' )
  pure $ x : xs

-- control expressions
ifExpr :: Parser Expr
ifExpr = do
  pos <- getSourcePos
  e <- If <$> (string if_ *> expr) <*> (string then_ *> expr) <*> (string else_ *> expr)
  pure $ At (SrcPos pos) e

lambda :: Parser Expr
lambda = do
  pos <- getSourcePos
  args <- between
          (trim $ string (\\))
          (trim $ string (-->))
           $ varName `sepEndBy1` spaces1
  e <- expr
  pure $ go pos e args
    where
      go _ e []     = e
      go p e (a:as) = At (SrcPos p) $ Lambda a $ go p e as


-- literatures
integer :: Parser Expr
integer = do
  num <- Lx.decimal
  pos <- getSourcePos
  pure $ At (SrcPos pos) (Lit $ Int num)

literature :: Parser Expr
literature = integer

-- terms
var :: Parser Expr
var = do
  sym <- varName
  when (sym `elem` reserved) (failure Nothing SE.empty)
  pos <- getSourcePos
  pure $ At (SrcPos pos) (Var sym)


term :: Parser Expr
term = literature <|> var <|> lambda <|> parens expr

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

expr :: Parser Expr
expr = trim $ ifExpr <|> apply <|> term



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
