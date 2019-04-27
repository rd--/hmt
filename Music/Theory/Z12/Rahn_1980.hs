-- | John Rahn. /Basic Atonal Theory/. Longman, New York, 1980.
module Music.Theory.Z12.Rahn_1980 where

import qualified Music.Theory.Z.Rahn_1980 as Rahn_1980 {- hmt -}
import qualified Music.Theory.Z12 as Z12 {- hmt -}

-- | 'Rahn_1980.rahn_prime' of 'id'.
--
-- > rahn_prime [0,1,3,6,8,9] == [0,2,3,6,7,9]
rahn_prime :: [Z12.Z12] -> [Z12.Z12]
rahn_prime = Rahn_1980.rahn_prime id
