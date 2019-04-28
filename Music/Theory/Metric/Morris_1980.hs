-- | Robert Morris. \"A Similarity Index for Pitch-Class
-- Sets\". Perspectives of New Music, 18(2):445-460, 1980.
module Music.Theory.Metric.Morris_1980 where

import Data.Int {- base -}
import Data.Ratio {- base -}

import Music.Theory.Z {- hmt -}
import Music.Theory.Z.Forte_1973 {- hmt -}

-- | SIM
--
-- > icv 12 [0,1,3,6] == [1,1,2,0,1,1] && icv 12 [0,2,4,7] == [0,2,1,1,2,0]
-- > sim [0,1,3,6] [0,2,4,7] == 6
-- > sim [0,1,2,4,5,8] [0,1,3,7] == 9
sim :: Integral a => [Int8] -> [Int8] -> a
sim r s =
    let r' = icv z12 r
        s' = icv z12 s
        t = zipWith (-) r' s'
    in sum (map abs t)

-- | ASIM
--
-- > asim [0,1,3,6] [0,2,4,7] == 6/12
-- > asim [0,1,2,4,5,8] [0,1,3,7] == 9/21
-- > asim [0,1,2,3,4] [0,1,4,5,7] == 2/5
-- > asim [0,1,2,3,4] [0,2,4,6,8] == 3/5
-- > asim [0,1,4,5,7] [0,2,4,6,8] == 3/5
asim :: (Integral n) => [Int8] -> [Int8] -> Ratio n
asim r s =
    let r' = icv z12 r
        s' = icv z12 s
    in sim r s % (sum r' + sum s')
