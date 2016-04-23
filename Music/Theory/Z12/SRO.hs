-- | Serial (ordered) pitch-class operations on 'Z12'.
module Music.Theory.Z12.SRO where

import Data.List {- base -}

import qualified Music.Theory.List as T
import qualified Music.Theory.Z.SRO as Z
import Music.Theory.Z12

-- | Transpose /p/ by /n/.
--
-- > tn 4 [1,5,6] == [5,9,10]
tn :: Z12 -> [Z12] -> [Z12]
tn = Z.tn z12_modulo

-- | Invert /p/ about /n/.
--
-- > invert 6 [4,5,6] == [8,7,6]
-- > invert 0 [0,1,3] == [0,11,9]
invert :: Z12 -> [Z12] -> [Z12]
invert = Z.invert z12_modulo

-- | Composition of 'invert' about @0@ and 'tn'.
--
-- > tni 4 [1,5,6] == [3,11,10]
-- > (invert 0 . tn  4) [1,5,6] == [7,3,2]
tni :: Z12 -> [Z12] -> [Z12]
tni = Z.tni z12_modulo

-- | Modulo 12 multiplication
--
-- > mn 11 [0,1,4,9] == tni 0 [0,1,4,9]
mn :: Z12 -> [Z12] -> [Z12]
mn = Z.mn z12_modulo

-- | M5, ie. 'mn' @5@.
--
-- > m5 [0,1,3] == [0,5,3]
m5 :: [Z12] -> [Z12]
m5 = mn 5

-- | T-related sequences of /p/.
--
-- > length (t_related [0,3,6,9]) == 12
t_related :: [Z12] -> [[Z12]]
t_related = Z.t_related z12_modulo

-- | T\/I-related sequences of /p/.
--
-- > length (ti_related [0,1,3]) == 24
-- > length (ti_related [0,3,6,9]) == 24
-- > ti_related [0] == map return [0..11]
ti_related :: [Z12] -> [[Z12]]
ti_related = Z.ti_related z12_modulo

-- | R\/T\/I-related sequences of /p/.
--
-- > length (rti_related [0,1,3]) == 48
-- > length (rti_related [0,3,6,9]) == 24
rti_related :: [Z12] -> [[Z12]]
rti_related = Z.rti_related z12_modulo

-- | T\/M\/I-related sequences of /p/, duplicates removed.
tmi_related :: [Z12] -> [[Z12]]
tmi_related p = let q = ti_related p in nub (q ++ map m5 q)

-- | R\/T\/M\/I-related sequences of /p/, duplicates removed.
rtmi_related :: [Z12] -> [[Z12]]
rtmi_related p = let q = tmi_related p in nub (q ++ map reverse q)

-- | r\/R\/T\/M\/I-related sequences of /p/, duplicates removed.
rrtmi_related :: [Z12] -> [[Z12]]
rrtmi_related p = nub (concatMap rtmi_related (T.rotations p))

-- * Sequence operations

-- | Variant of 'tn', transpose /p/ so first element is /n/.
--
-- > tn_to 5 [0,1,3] == [5,6,8]
-- > map (tn_to 0) [[0,1,3],[1,3,0],[3,0,1]] == [[0,1,3],[0,2,11],[0,9,10]]
tn_to :: Z12 -> [Z12] -> [Z12]
tn_to = Z.tn_to z12_modulo

-- | Variant of 'invert', inverse about /n/th element.
--
-- > map (invert_ix 0) [[0,1,3],[3,4,6]] == [[0,11,9],[3,2,0]]
-- > map (invert_ix 1) [[0,1,3],[3,4,6]] == [[2,1,11],[5,4,2]]
invert_ix :: Int -> [Z12] -> [Z12]
invert_ix = Z.invert_ix z12_modulo

-- | The standard t-matrix of /p/.
--
-- > tmatrix [0,1,3] == [[0,1,3]
-- >                    ,[11,0,2]
-- >                    ,[9,10,0]]
tmatrix :: [Z12] -> [[Z12]]
tmatrix = Z.tmatrix z12_modulo
