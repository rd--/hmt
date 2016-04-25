-- | Pitch-class set (unordered) operations on 'Z12'.
module Music.Theory.Z12.TTO where

import Data.List {- base -}

import Music.Theory.Z12

-- | Map to pitch-class and reduce to set.
--
-- > pcset [1,13] == [1]
pcset :: (Integral a) => [a] -> [Z12]
pcset = nub . sort . map fromIntegral

-- | Transpose by n.
--
-- > tto_tn 4 [1,5,6] == [5,9,10]
-- > tto_tn 4 [0,4,8] == [0,4,8]
tto_tn :: Z12 -> [Z12] -> [Z12]
tto_tn n = sort . map (+ n)

-- | Invert about n.
--
-- > tto_invert 6 [4,5,6] == [6,7,8]
-- > tto_invert 0 [0,1,3] == [0,9,11]
tto_invert :: Z12 -> [Z12] -> [Z12]
tto_invert n = sort . map (\p -> n - (p - n))

-- | Composition of 'invert' about @0@ and 'tn'.
--
-- > tto_tni 4 [1,5,6] == [3,10,11]
-- > (tto_invert 0 . tto_tn 4) [1,5,6] == [2,3,7]
tto_tni :: Z12 -> [Z12] -> [Z12]
tto_tni n = tto_tn n . tto_invert 0

-- | Modulo 12 multiplication
--
-- > tto_mn 11 [0,1,4,9] == tto_invert 0 [0,1,4,9]
tto_mn :: Z12 -> [Z12] -> [Z12]
tto_mn n = sort . map (* n)

-- | M5, ie. 'mn' @5@.
--
-- > tto_m5 [0,1,3] == [0,3,5]
tto_m5 :: [Z12] -> [Z12]
tto_m5 = tto_mn 5

-- | T-related sets of /p/.
--
-- > length (tto_t_related [0,1,3]) == 12
-- > tto_t_related [0,3,6,9] == [[0,3,6,9],[1,4,7,10],[2,5,8,11]]
tto_t_related :: [Z12] -> [[Z12]]
tto_t_related p = nub (map (`tto_tn` p) [0..11])

-- | T\/I-related set of /p/.
--
-- > length (tto_ti_related [0,1,3]) == 24
-- > tto_ti_related [0,3,6,9] == [[0,3,6,9],[1,4,7,10],[2,5,8,11]]
tto_ti_related :: [Z12] -> [[Z12]]
tto_ti_related p = nub (tto_t_related p ++ tto_t_related (tto_invert 0 p))
