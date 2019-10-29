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
  , topLevel
  , Parser
  ) where

import           Hrafnar.Annotation
import           Hrafnar.AST
import           Hrafnar.Exception
import           Hrafnar.Types

import           Control.Monad
import           Data.Functor
import qualified Data.List                  as L
import           Data.Maybe
import qualified Data.Set                   as SE
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lx

type Parser = Parsec ParserException String


-- | Reserved words.
if_, then_, else_, let_, in_, data_, case_, of_ :: String
if_ = "if"
then_ = "then"
else_ = "else"
let_ = "let"
in_ = "in"
data_ = "data"
case_ = "case"
of_ = "of"

-- | List of reserved words.
reservedWords :: [String]
reservedWords =
  [ if_
  , then_
  , else_
  , let_
  , in_
  , data_
  , case_
  , of_
  ]

-- | Reserved symbols.
(-\), (-->), (-=), (-:), (-|), comma, underscore :: String
(-\) = "\\"
(-->) = "->"
(-=) = "="
(-:) = ":"
(-|) = "|"
comma = ","
underscore = "_"

-- | List of reserved symbols.
reservedSymbols :: [String]
reservedSymbols =
  [ (-\)
  , (-->)
  , (-=)
  , (-:)
  , (-|)
  , comma
  , underscore
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

parens :: Parser a -> Parser a
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
scn = Lx.space space1 lineComment blockComment

sc :: Parser () -- sc means space consumer
sc = Lx.space (void spaces1) lineComment blockComment

data ConParam
  = TypeName String
  | VarName String

-- signatures
-- | Variable names start on lower character.
varName :: Parser String
varName = do
  x <- lowerChar
  xs <- many (alphaNumChar <|> char '_' <|> char '\'' )
  when (x : xs `elem` reservedWords) (fancyFailure . SE.singleton $ ErrorCustom ReservedKeyWord)
  lexeme . pure $ x : xs

-- | Type name starts on upper character.
typeName :: Parser String
typeName = do
  x <- upperChar
  xs <- many (alphaNumChar <|> char '_' <|> char '\'' )
  lexeme . pure $ x : xs

-- | Synonym of typeName.
dataName :: Parser String
dataName = typeName

-- | Definition of operators.
--   NOTE: when use '?', want to force to return Maybe value.
operator :: Parser String
operator = do
  op <- some
    ( oneOf ("!#$%&*+./<=>?@\\^|-~" :: String) <|>
      symbolChar
    )
  when (op `elem` reservedSymbols) (fancyFailure . SE.singleton $ ErrorCustom ReservedSymbol)
  lexeme $ pure op

-- control expressions
-- | "if" expression.
ifExpr :: Parser Expr
ifExpr = do
  pos <- getSourcePos
  e <- If <$> (symbol if_ *> expr) <*> (symbol then_ *> expr) <*> (symbol else_ *> expr)
  pure $ At (SrcPos pos) e

-- | Lambda expression. it allows to write two followings.
--   a) f = \x -> x + x
--   b) f = \x ->
--            x + x
--  NOTE: Want to allow following expression, but can't solve now.
--        Pay attention to indent level is less than lambda.
--   c) f = \x ->
--        x + x
lambda :: Parser Expr
lambda = do
  pos <- getSourcePos
  args <- between
          (symbol (-\))
          (symbol (-->))
           $ varName `sepEndBy1` sc
  let level = sourceColumn pos
  e <- try expr <|> (eol *> Lx.indentGuard sc GT level *> expr)
  pure $ go pos e args
    where
      go _ e []     = e
      go p e (a:as) = At (SrcPos p) $ Lambda a $ go p e as

-- | "let ~ in ~" expression. Available with indent.
--   TODO: Use indent level like lambda expression.
--         This implementation is little bit complecated.
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

-- | "case" expression. Its branches allow to indent.
caseExpr :: Parser Expr
caseExpr = Lx.indentBlock scn parser
  where
    parser = do
      pos <- getSourcePos
      _ <- symbol case_
      e <- lexeme expr
      _ <- symbol of_
      pure $ Lx.IndentSome Nothing
        (pure . At (SrcPos pos) . Case e)
        (try branch <|> indentedBranch)

    branch = do
      varPos <- getSourcePos
      p <- pat
      _ <- symbol (-->)
      e <- expr
      pure (At (SrcPos varPos) p, e)

    indentedBranch = do
      varPos <- getSourcePos
      (p, e:es) <- Lx.indentBlock scn (
        do
          p <- pat
          Lx.IndentSome Nothing (\es -> pure (p, es)) expr <$ symbol (-->)
        )
      unless (L.null es) $ failure Nothing SE.empty
      pure (At (SrcPos varPos) p, e)

    pat = try (PVar <$> varName) <|>
          try patternLiteral <|>
          try conParser <|>
          PWildcard <$ symbol underscore

    conParser = do
      pos <- getSourcePos
      con <- typeName
      params <- (At (SrcPos pos) <$> (lexeme (parens conParser) <|> pat)) `sepBy` sc
      pure $ PCon con params

-- literatures
integer :: (Lit -> a) -> Parser a
integer f = do
  num <- lexeme Lx.decimal
  pure (f $ Int num)

literal :: Parser Expr
literal = do
  pos <- getSourcePos
  At (SrcPos pos) <$> integer Lit

patternLiteral :: Parser Pat'
patternLiteral = integer PLit

-- terms
-- | Variables include data constructors and operators.
var :: Parser Expr
var = do
  name <- try varName <|> try operator <|> dataName
  pos <- getSourcePos
  pure $ At (SrcPos pos) (Var name)


-- | Term.
term :: Parser Expr
term = literal <|> try var <|> lambda <|> parens expr

-- | Application a term to another term.
--   This syntax has "Left recursion".
apply :: Parser Expr
apply = do
  start <- getSourcePos
  t <- term
  loop start t
  where
    loop s e = try (loop' s e) <|> pure e
    loop' s lhs = do
      pos <- getSourcePos
      op <- lexeme $ pure (\e1 e2 -> At (SrcPos pos) (Apply e1 e2))
      Lx.indentGuard sc GT (sourceColumn s)
      rhs <- term
      loop s $ op lhs rhs

-- | Expression.
expr :: Parser Expr
expr = lexeme $ ifExpr <|> letExpr <|> caseExpr <|> apply <|> term

-- declarations
-- | Declaration of expression like following.
--     x = 1 + 2
--   This syntax allows indented block.
--     x =
--       1 + 2
exprDecl :: Parser Decl
exprDecl = do
  pos <- getSourcePos
  let level = sourceColumn pos
  name <- varName
  _ <- symbol (-=)
  e <- try expr <|> (eol *> Lx.indentGuard sc GT level *> expr)
  pure . At (SrcPos pos) $ ExprDecl name e

-- | Type anotation like following.
--     f : Either String a -> Int -> Maybe a
typeAnno :: Parser Decl
typeAnno = do
  names <- varName `sepBy1` symbol comma
  _ <- symbol (-:)
  types <- typesP
  let typ = foldr1 TyFun types
  pos <- getSourcePos
  pure . At (SrcPos pos) $ TypeAnno names typ
    where
      typeP = TyCon <$> typeName <*>
              ( TyCon <$> typeName <*> pure [] <|>
                TyCon <$> typeName <*> parens typesP <|>
                TyVar . TV <$> varName <|>
                parens typeP
              ) `sepBy` sc <|>
              TyVar . TV <$> varName <|>
              foldr1 TyFun <$> parens typesP

      typesP = typeP `sepBy1` symbol (-->)

-- | Declaration of data constructor.
--     data Maybe a = Just a | Nothing
dataDecl :: Parser Decl
dataDecl = do
  _ <- symbol data_
  tyName <- dataName
  tyVars <- varName `sepBy` sc
  _ <- symbol (-=)
  let p = do
         conName <- typeName
         params <- (TypeName <$> typeName <|> VarName <$> varName) `sepBy` sc
         pure (conName, foldr (\case
                                  TypeName x -> TyFun (TyCon x [])
                                  VarName x -> TyFun (TyVar (TV x))
                              ) (TyCon tyName $ fmap (TyVar . TV) tyVars) params
              )
  cons <- p `sepBy` symbol (-|)
  pos <- getSourcePos
  pure . At (SrcPos pos) $ DataDecl tyName tyVars cons

decl :: Parser Decl
decl = try exprDecl <|> try typeAnno <|> dataDecl

topLevel :: Parser [Decl]
topLevel = catMaybes <$> some (try (Just <$> decl) <|> try (Just <$> decl <* newline) <|> newline $> Nothing) <* eof

exprParser :: Parser Expr
exprParser = expr

declParser :: Parser Decl
declParser = decl


