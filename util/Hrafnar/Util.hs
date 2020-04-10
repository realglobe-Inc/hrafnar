module Hrafnar.Util
  ( rawS
  , rawC
  ) where

import           Data.Void

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as S
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void String

genQQ :: S.Lift a => (String -> Either String a) -> QuasiQuoter
genQQ parser =
  QuasiQuoter
    { quoteExp = quoteExp . parser
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }
  where
    quoteExp :: S.Lift a => Either String a -> Q Exp
    quoteExp (Left x) = fail x
    quoteExp (Right x) = S.lift x

-- raw string
rawS :: QuasiQuoter
rawS = genQQ parser
  where
    parser :: String -> Either String String
    parser s = case parse closingMarkOfQQ "" s of -- TODO: determine file name
      Left err -> Left $ errorBundlePretty err
      Right res -> Right res

-- raw char
rawC :: QuasiQuoter
rawC = genQQ toChar
  where
    toChar :: String -> Either String Char
    toChar [] = Left "no character"
    toChar [x] = Right x
    toChar (_:_) = Left "multiple characters"

closingMarkOfQQ :: Parser String
closingMarkOfQQ = do
  bar <- char '|'
  _:restWaves <- some (char '~')
  closingBracket <- char ']'
  return $ [bar] <> restWaves <> [closingBracket]
