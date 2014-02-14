-- | Math functions.
module Music.Theory.Math where

import Data.Ratio {- base -}

-- | Pretty printer for 'Rational' that elides denominators of @1@.
--
-- > map rational_pp [1,3/2,2] == ["1","3/2","2"]
rational_pp :: Rational -> String
rational_pp r =
    let n = numerator r
        d = denominator r
    in if d == 1
       then show n
       else concat [show n,"/",show d]
