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

import           Text.Megaparsec
import Data.Void

type Parser = Parsec Void String

exprParser :: Parser Expr
exprParser = undefined

declsParser :: Parser [Decl]
declsParser = undefined

lineParser :: Parser Line
lineParser = undefined
