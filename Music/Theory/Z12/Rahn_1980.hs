-- | John Rahn. /Basic Atonal Theory/. Longman, New York, 1980.
module Music.Theory.Z12.Rahn_1980 where

import Music.Theory.Z12
import qualified Music.Theory.Z.Forte_1973 as Z

-- | Rahn prime form (comparison is rightmost inwards).
--
-- > rahn_cmp [0,1,3,6,8,9] [0,2,3,6,7,9] == GT
rahn_cmp :: Ord a => [a] -> [a] -> Ordering
rahn_cmp p q = compare (reverse p) (reverse q)

-- | Rahn prime form, ie. 'ti_cmp_prime' of 'rahn_cmp'.
--
-- > rahn_prime [0,1,3,6,8,9] == [0,2,3,6,7,9]
--
-- > import Music.Theory.Z12.Forte_1973
--
-- > let s = [[0,1,3,7,8]
-- >         ,[0,1,3,6,8,9],[0,1,3,5,8,9]
-- >         ,[0,1,2,4,7,8,9]
-- >         ,[0,1,2,4,5,7,9,10]]
-- > in all (\p -> forte_prime p /= rahn_prime p) s == True
rahn_prime :: [Z12] -> [Z12]
rahn_prime = Z.ti_cmp_prime id rahn_cmp
