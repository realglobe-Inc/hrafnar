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
rawC = genQQ head
-- HACK: `head` does not raise an error when a given string is more than
-- 1 charcter. It smells bad.
