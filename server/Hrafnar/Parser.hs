{-|
Description : Parser of hrafnar-lang
Module      : Hrafnar.Parser
Copyright   : REALGLOBE INC. (c) REALGLOBE 2018
License     : BSD3

Maintainer  : REALGLOBE INC.
-}

module Hrafnar.Parser
  ( exprParser
  , declParser
  , lineParser
  , declsParser
  ) where

import           Hrafnar.Annotation
import           Hrafnar.AST
import           Hrafnar.Types

import           Control.Monad
import           Data.Functor
import qualified Data.List                  as L
import qualified Data.Set                   as SE
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lx

type Parser = Parsec Void String

declsParser = undefined -- FIXME: temporary definition

-- | Reserved words.
if_, then_, else_, let_, in_ :: String
if_ = "if"
then_ = "then"
else_ = "else"
let_ = "let"
in_ = "in"

-- | List of reserved words.
reservedWords :: [String]
reservedWords =
  [ if_
  , then_
  , else_
  , let_
  , in_
  ]

-- | Reserved symbols.
(-\), (-->), (-=), (-:), comma :: String
(-\) = "\\"
(-->) = "->"
(-=) = "="
(-:) = ":"
comma = ","

-- | List of reserved symbols.
reservedSymbols :: [String]
reservedSymbols =
  [ (-\)
  , (-->)
  , (-=)
  , (-:)
  , comma
  ]

-- misc
sp :: Parser Char
sp = char ' ' <|> char '\t'

spaces :: Parser String
spaces = many sp

spaces1 :: Parser String
spaces1 = some sp

trim :: Parser a -> Parser a
trim = between spaces spaces

parens :: Parser Expr -> Parser Expr
parens = between (char '(' <* space) (space *> char ')')

lineComment :: Parser ()
lineComment = Lx.skipLineComment "--"

blockComment :: Parser ()
blockComment = Lx.skipBlockCommentNested "{-" "-}"

symbol :: String -> Parser String
symbol = trim . Lx.symbol sc

lexeme :: Parser a -> Parser a
lexeme = Lx.lexeme sc

scn :: Parser ()
scn = Lx.space space1 lineComment empty

sc :: Parser () -- sc means space copnsumer
sc = Lx.space (void spaces1) lineComment blockComment

-- signatures
varName :: Parser String
varName = do
  x <- lowerChar
  xs <- many (alphaNumChar <|> char '_' <|> char '\'' )
  when (x : xs `elem` reservedWords) (failure Nothing SE.empty)
  pure $ x : xs

typeName :: Parser String
typeName = do
  x <- upperChar
  xs <- many (alphaNumChar <|> char '_' <|> char '\'' )
  when (x : xs `elem` reservedWords) (failure Nothing SE.empty)
  pure $ x : xs
  
dataName :: Parser String
dataName = typeName

-- control expressions
ifExpr :: Parser Expr
ifExpr = do
  pos <- getSourcePos
  e <- If <$> (symbol if_ *> expr) <*> (symbol then_ *> expr) <*> (symbol else_ *> expr)
  pure $ At (SrcPos pos) e

lambda :: Parser Expr
lambda = do
  pos <- getSourcePos
  args <- between
          (symbol (-\))
          (symbol (-->))
           $ varName `sepEndBy1` sc
  e <- expr
  pure $ go pos e args
    where
      go _ e []     = e
      go p e (a:as) = At (SrcPos p) $ Lambda a $ go p e as

letExpr :: Parser Expr
letExpr =
  let
    inlineLetIn = symbol let_ *> sc *> ((:[]) <$> decl) <* sc <* symbol in_
    blockLetIn = Lx.indentBlock scn (symbol let_ *> spaces $> Lx.IndentSome Nothing pure decl) <* symbol in_
    extractLet p = do
      (ds, e:es) <- Lx.indentBlock scn (
        do
          decls <- p
          pure $  Lx.IndentSome Nothing (\exprs -> pure (decls, exprs)) expr
        )
      unless (L.null es) $ failure Nothing SE.empty
      pure $ Let ds e
  in do
    pos <- getSourcePos
    try (At (SrcPos pos) <$> (Let <$> inlineLetIn <*> expr)) <|>
      try (At (SrcPos pos) <$> extractLet inlineLetIn) <|>
      try (At (SrcPos pos) <$> (Let <$> blockLetIn <*> expr)) <|>
      At (SrcPos pos) <$> extractLet blockLetIn


-- literatures
integer :: Parser Expr
integer = do
  num <- lexeme Lx.decimal
  pos <- getSourcePos
  pure $ At (SrcPos pos) (Lit $ Int num)

literature :: Parser Expr
literature = integer

-- terms
var :: Parser Expr
var = do
  name <- lexeme varName
  pos <- getSourcePos
  pure $ At (SrcPos pos) (Var name)


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
expr = trim $ ifExpr <|> letExpr <|> apply <|> term

-- declarations
exprDecl :: Parser Decl
exprDecl = do
  name <- lexeme varName
  _ <- symbol (-=)
  e <- expr
  pos <- getSourcePos
  pure . At (SrcPos pos) $ ExprDecl name e

typeAnno :: Parser Decl
typeAnno = do
  names <- lexeme varName `sepBy1` symbol comma
  _ <- symbol (-:)
  typ <- lexeme typeName
  pos <- getSourcePos
  pure . At (SrcPos pos) $ TypeAnno names (TyCon typ)
  

dataDecl :: Parser Decl
dataDecl = undefined

decl :: Parser Decl
decl = try exprDecl <|> typeAnno -- <|> dataDecl


exprParser :: Parser Expr
exprParser = expr

declParser :: Parser Decl
declParser = decl

lineParser :: Parser Line
lineParser = undefined
