module Music.Theory.Z12.Morris_1987 where

import qualified Music.Theory.List as L
import Music.Theory.Z12

-- | @INT@ operator.
--
-- > int [0,1,3,6,10] == [1,2,3,4]
int :: [Z12] -> [Z12]
int = L.d_dx
