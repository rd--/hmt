-- | Serial (ordered) pitch-class operations on 'Z12'.
module Music.Theory.Z12.SRO where

import Data.List
import qualified Music.Theory.List as T
import Music.Theory.Z12

-- | Transpose /p/ by /n/.
--
-- > tn 4 [1,5,6] == [5,9,10]
tn :: Z12 -> [Z12] -> [Z12]
tn n = fmap (+ n)

-- | Invert /p/ about /n/.
--
-- > invert 6 [4,5,6] == [8,7,6]
-- > invert 0 [0,1,3] == [0,11,9]
invert :: Z12 -> [Z12] -> [Z12]
invert n = fmap (\p -> n - (p - n))

-- | Composition of 'invert' about @0@ and 'tn'.
--
-- > tni 4 [1,5,6] == [3,11,10]
-- > (invert 0 . tn  4) [1,5,6] == [7,3,2]
tni :: Z12 -> [Z12] -> [Z12]
tni n = tn n . invert 0

-- | Modulo 12 multiplication
--
-- > mn 11 [0,1,4,9] == tni 0 [0,1,4,9]
mn :: Z12 -> [Z12] -> [Z12]
mn n = fmap (* n)

-- | M5, ie. 'mn' @5@.
--
-- > m5 [0,1,3] == [0,5,3]
m5 :: [Z12] -> [Z12]
m5 = mn 5

-- | T-related sequences of /p/.
--
-- > length (t_related [0,3,6,9]) == 12
t_related :: [Z12] -> [[Z12]]
t_related p = fmap (`tn` p) [0..11]

-- | T\/I-related sequences of /p/.
--
-- > length (ti_related [0,1,3]) == 24
-- > length (ti_related [0,3,6,9]) == 24
-- > ti_related [0] == map return [0..11]
ti_related :: [Z12] -> [[Z12]]
ti_related p = nub (t_related p ++ t_related (invert 0 p))

-- | R\/T\/I-related sequences of /p/.
--
-- > length (rti_related [0,1,3]) == 48
-- > length (rti_related [0,3,6,9]) == 24
rti_related :: [Z12] -> [[Z12]]
rti_related p = let q = ti_related p in nub (q ++ map reverse q)

-- | T\/M\/I-related sequences of /p/.
tmi_related :: [Z12] -> [[Z12]]
tmi_related p = let q = ti_related p in nub (q ++ map m5 q)

-- | R\/T\/M\/I-related sequences of /p/.
rtmi_related :: [Z12] -> [[Z12]]
rtmi_related p = let q = tmi_related p in nub (q ++ map reverse q)

-- | r\/R\/T\/M\/I-related sequences of /p/.
rrtmi_related :: [Z12] -> [[Z12]]
rrtmi_related p = nub (concatMap rtmi_related (T.rotations p))

-- * Sequence operations

-- | Variant of 'tn', transpose /p/ so first element is /n/.
--
-- > tn_to 5 [0,1,3] == [5,6,8]
tn_to :: Z12 -> [Z12] -> [Z12]
tn_to n p =
    case p of
      [] -> []
      x:xs -> n : tn (n - x) xs

-- | Variant of 'invert', inverse about /n/th element.
--
-- > map (invert_ix 0) [[0,1,3],[3,4,6]] == [[0,11,9],[3,2,0]]
-- > map (invert_ix 1) [[0,1,3],[3,4,6]] == [[2,1,11],[5,4,2]]
invert_ix :: Int -> [Z12] -> [Z12]
invert_ix n p = invert (p!!n) p

-- | The standard t-matrix of /p/.
--
-- > tmatrix [0,1,3] == [[0,1,3]
-- >                    ,[11,0,2]
-- >                    ,[9,10,0]]
tmatrix :: [Z12] -> [[Z12]]
tmatrix p = map (`tn` p) (tn_to 0 (invert_ix 0 p))
