-- | Robert Morris. \"A Similarity Index for Pitch-Class
-- Sets\". Perspectives of New Music, 18(2):445-460, 1980.
module Music.Theory.Metric.Morris_1980 where

import Data.Ratio
import qualified Music.Theory.PitchClass as T

-- | SIM
--
-- > T.icv [0,1,3,6] == [1,1,2,0,1,1] && T.icv [0,2,4,7] == [0,2,1,1,2,0]
-- > sim [0,1,3,6] [0,2,4,7] == 6
-- > sim [0,1,2,4,5,8] [0,1,3,7] == 9
sim :: Integral a => [a] -> [a] -> a
sim r s =
    let r' = T.icv r
        s' = T.icv s
        t = zipWith (-) r' s'
    in sum (map abs t)

-- | ASIM
--
-- > asim [0,1,3,6] [0,2,4,7] == 6/12
-- > asim [0,1,2,4,5,8] [0,1,3,7] == 9/21
-- > asim [0,1,2,3,4] [0,1,4,5,7] == 2/5
-- > asim [0,1,2,3,4] [0,2,4,6,8] == 3/5
-- > asim [0,1,4,5,7] [0,2,4,6,8] == 3/5
asim :: (Integral n) => [n] -> [n] -> Ratio n
asim r s =
    let r' = T.icv r
        s' = T.icv s
    in fromIntegral (sim r s) % fromIntegral (sum r' + sum s')
