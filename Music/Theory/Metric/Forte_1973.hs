-- | Allen Forte. The Structure of Atonal Music. Yale University
-- Press, New Haven, 1973.
module Music.Theory.Metric.Forte_1973 where

import Data.Maybe
import Data.List
import qualified Music.Theory.List as L
import qualified Music.Theory.Set as S
import Music.Theory.Z12

-- | Basic interval pattern, see Allen Forte \"The Basic Interval Patterns\"
-- /JMT/ 17/2 (1973):234-272
--
-- >>> bip 0t95728e3416
-- 11223344556
--
-- > bip [0,10,9,5,7,2,8,11,3,4,1,6] == [1,1,2,2,3,3,4,4,5,5,6]
-- > bip (pco "0t95728e3416") == [1,1,2,2,3,3,4,4,5,5,6]
bip :: [Z12] -> [Z12]
bip = sort . map ic . L.d_dx

-- | Interval class of Z12 interval /i/.
--
-- > map ic [5,6,7] == [5,6,5]
ic :: Z12 -> Z12
ic i = if i <= 6 then i else 12 - i

-- | Forte notation for interval class vector.
--
-- > icv [0,1,2,4,7,8] == [3,2,2,3,3,2]
icv :: Integral i => [Z12] -> [i]
icv s =
    let i = map (ic . uncurry (-)) (S.dyads s)
        j = map f (group (sort i))
        k = map (`lookup` j) [1..6]
        f l = (head l,genericLength l)
    in map (fromMaybe 0) k
