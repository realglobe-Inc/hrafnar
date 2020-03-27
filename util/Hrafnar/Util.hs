module Hrafnar.Util
  ( rawS
  , rawC
  ) where

import           Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as S

genQQ :: S.Lift a => (String -> a) -> QuasiQuoter
genQQ parseFunc =
  QuasiQuoter
    { quoteExp = S.lift . parseFunc
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }

-- raw string
rawS :: QuasiQuoter
rawS = genQQ id

-- raw char
rawC :: QuasiQuoter
rawC = genQQ toChar
  where
    toChar :: String -> Char
    toChar [] = error "no character"
    toChar [x] = x
    toChar (_:_) = error "multiple characters"
