{
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Hrafnar.Lexer where

import Hrafnar.Annotation
import Hrafnar.Assoc

import Control.Monad.State
import qualified Data.Map.Strict as MA
import qualified Data.Text as ST
import Data.Scientific
}

%wrapper "monadUserState"

$digit = 0-9
$smallalpha = a-z
$largealpha = A-Z
$alpha = [$smallalpha$largealpha]
$symbol = [\+\-\*\/]

tokens :-

<0>  $white # \n + { skip } --{ mkLx LxWhite }

-- | Literals
<0>  $digit+ { mkLx LxInt }

-- | Reserved symbols
<0>  [\;\n] { mkLx LxSep }
<0>  \= { mkLx LxEqual }
<0>  \( { mkLx LxLParen }
<0>  \) { mkLx LxRParen }
<0>  \{ { mkLx LxLBrace }
<0>  \} { mkLx LxRBrace }
<0>  \\ { mkLx LxLambda }
<0>  \-\> { mkLx LxArrow }
<0>  \| { mkLx LxVLine }
<0>  \, { mkLx LxComma }
<0>  \: { mkLx LxColon }
<0>  \_ { mkLx LxUnderscore }

-- | Reserved keywords
<0>  let { mkLx LxLet }
<0>  in { mkLx LxIn }
<0>  if {mkLx LxIf }
<0>  then {mkLx LxThen }
<0>  else {mkLx LxElse }
<0>  data { mkLx LxData }
<0>  case { mkLx LxCase }
<0>  of { mkLx LxOf }
<0>  do { mkLx LxDo }

-- | Data constructors
<0>  $largealpha[$alpha$digit\']* { mkLx LxConId }

-- | Variables
<0>  $smallalpha[$alpha$digit\']*  { mkLx LxVarId }
<0>  [\+\-\*\/\^]+           { mkLx LxVarSym }
{
data Lexeme

  -- | Symbols
  = LxWhite
  | LxSep
  | LxEqual
  | LxLParen
  | LxRParen
  | LxLBrace
  | LxRBrace
  | LxLambda
  | LxArrow
  | LxVLine
  | LxComma
  | LxColon
  | LxUnderscore

  -- | Literals.
  | LxVarId
  | LxConId
  | LxVarSym
  | LxInt

  -- | Reserved keywords.
  | LxLet
  | LxIn
  | LxIf
  | LxThen
  | LxElse
  | LxData
  | LxCase
  | LxOf
  | LxDo
  deriving (Eq, Show)

data Token

  -- | Symbols
  = TkWhite Position
  | TkSep Position
  | TkEqual Position
  | TkLParen Position
  | TkRParen Position
  | TkLBrace Position
  | TkRBrace Position
  | TkVLine Position
  | TkArrow Position
  | TkComma Position
  | TkColon Position
  | TkLambda Position
  | TkUnderscore Position

  -- | Literals.
  | TkVarId (String, Position)
  | TkConId (String, Position)
  | TkVarSym ((String, Int, OpAssoc), Position) -- TODO: Use monadic method
  | TkInt (Int, Position)

  -- | Reserved keywords.
  | TkLet Position
  | TkIn Position
  | TkIf Position
  | TkThen Position
  | TkElse Position
  | TkData Position
  | TkCase Position
  | TkOf Position
  | TkDo Position
  | TkEof
  deriving (Eq, Show)

mkLx :: Lexeme -> AlexInput -> Int -> Alex Token
mkLx lx (alexPos, _, _, str) len =
  let
    t = take len str
    pos = alexToPos alexPos
  in
    case lx of

      -- | Symbols.
      LxWhite -> pure $ TkWhite pos
      LxSep -> pure $ TkSep pos
      LxEqual -> pure $ TkEqual pos
      LxLParen -> pure $ TkLParen pos
      LxRParen -> pure $ TkRParen pos
      LxLBrace -> pure $ TkLBrace pos
      LxRBrace -> pure $ TkRBrace pos
      LxArrow -> pure $ TkArrow pos
      LxVLine -> pure $ TkVLine pos
      LxComma -> pure $ TkComma pos
      LxColon -> pure $ TkColon pos
      LxLambda -> pure $ TkLambda pos
      LxUnderscore -> pure $ TkUnderscore pos

      -- | Literals.
      LxVarId -> pure $ TkVarId (t, pos)
      LxConId -> pure $ TkConId (t, pos)
      LxVarSym -> Alex $
        (\s@AlexState{..} ->
            case MA.lookup t (lxOps alex_ust) of
              Just (n, asoc) -> Right (s, TkVarSym ((t, fromIntegral n, asoc), pos))
              _ -> Left "unknown operator"
        )
      LxInt -> pure $ TkInt (read t,  pos)

      -- | Reserved keywords.
      LxLet -> pure $ TkLet pos
      LxIn -> pure $ TkIn pos
      LxIf -> pure $ TkIf pos
      LxThen -> pure $ TkThen pos
      LxElse -> pure $ TkElse pos
      LxData -> pure $ TkData pos
      LxCase -> pure $ TkCase pos
      LxOf -> pure $ TkOf pos
      LxDo -> pure $ TkDo pos

alexEOF :: Alex Token
alexEOF = pure TkEof


data AlexUserState = AlexUserState
  { lxOps :: MA.Map String (Int, OpAssoc)
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState MA.empty

alexToPos :: AlexPosn -> Position
alexToPos (AlexPn _ l c) = SrcPos l c
}
