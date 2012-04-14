-- | Pitch-class set operations on 'Z12' sets.
module Music.Theory.Z12.TTO where

import Data.List
import Music.Theory.Z12

-- | Map to pitch-class and reduce to set.
--
-- > pcset [1,13] == [1]
pcset :: (Integral a) => [a] -> [Z12]
pcset = nub . sort . map fromIntegral

-- | Transpose by n.
--
-- > tn 4 [1,5,6] == [5,9,10]
-- > tn 4 [0,4,8] == [0,4,8]
tn :: Z12 -> [Z12] -> [Z12]
tn n = sort . map (+ n)

-- | Invert about n.
--
-- > invert 6 [4,5,6] == [6,7,8]
-- > invert 0 [0,1,3] == [0,9,11]
invert :: Z12 -> [Z12] -> [Z12]
invert n = sort . map (\p -> n - (p - n))

-- | Composition of 'invert' about @0@ and 'tn'.
--
-- > tni 4 [1,5,6] == [3,10,11]
-- > (invert 0 . tn  4) [1,5,6] == [2,3,7]
tni :: Z12 -> [Z12] -> [Z12]
tni n = tn n . invert 0

-- | Modulo 12 multiplication
--
-- > mn 11 [0,1,4,9] == invert 0 [0,1,4,9]
mn :: Z12 -> [Z12] -> [Z12]
mn n = sort . map (* n)

-- | M5, ie. 'mn' @5@.
--
-- > m5 [0,1,3] == [0,3,5]
m5 :: [Z12] -> [Z12]
m5 = mn 5

-- | T-related sets of /p/.
--
-- > t_related [0,3,6,9] == [[0,3,6,9],[1,4,7,10],[2,5,8,11]]
t_related :: [Z12] -> [[Z12]]
t_related p = nub (map (`tn` p) [0..11])

-- | T/I-related set of /p/.
--
-- > length (ti_related [0,1,3]) == 24
-- > length (ti_related [0,3,6,9]) == 3
ti_related :: [Z12] -> [[Z12]]
ti_related p = nub (t_related p ++ t_related (invert 0 p))
