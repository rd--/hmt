-- | Robert Morris. \"A Similarity Index for Pitch-Class
-- Sets\". Perspectives of New Music, 18(2):445-460, 1980.
module Music.Theory.Metric.Morris_1980 where

import Data.Ratio
import qualified Music.Theory.Metric.Forte_1973 as F
import Music.Theory.Z12

-- | SIM
--
-- > F.icv [0,1,3,6] == [1,1,2,0,1,1] && F.icv [0,2,4,7] == [0,2,1,1,2,0]
-- > sim [0,1,3,6] [0,2,4,7] == 6
-- > sim [0,1,2,4,5,8] [0,1,3,7] == 9
sim :: Integral a => [Z12] -> [Z12] -> a
sim r s =
    let r' = F.icv r
        s' = F.icv s
        t = zipWith (-) r' s'
    in sum (map abs t)

-- | ASIM
--
-- > asim [0,1,3,6] [0,2,4,7] == 6/12
-- > asim [0,1,2,4,5,8] [0,1,3,7] == 9/21
-- > asim [0,1,2,3,4] [0,1,4,5,7] == 2/5
-- > asim [0,1,2,3,4] [0,2,4,6,8] == 3/5
-- > asim [0,1,4,5,7] [0,2,4,6,8] == 3/5
asim :: (Integral n) => [Z12] -> [Z12] -> Ratio n
asim r s =
    let r' = F.icv r
        s' = F.icv s
    in sim r s % (sum r' + sum s')
