module Hrafnar.Util
  ( rawS
  , rawC
  ) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as S

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
rawS = genQQ Right

-- raw char
rawC :: QuasiQuoter
rawC = genQQ toChar
  where
    toChar :: String -> Either String Char
    toChar [] = Left "no character"
    toChar [x] = Right x
    toChar (_:_) = Left "multiple characters"
