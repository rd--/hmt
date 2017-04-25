-- | Serial (ordered) pitch-class operations on 'Z12'.
module Music.Theory.Z12.SRO where

import Data.List {- base -}

import qualified Music.Theory.List as T
import qualified Music.Theory.Z.SRO as Z
import           Music.Theory.Z12

-- | Transpose /p/ by /n/.
--
-- > sro_tn 4 [1,5,6] == [5,9,10]
sro_tn :: Z12 -> [Z12] -> [Z12]
sro_tn = Z.z_sro_tn id

-- | Invert /p/ about /n/.
--
-- > sro_invert 6 [4,5,6] == [8,7,6]
-- > sro_invert 0 [0,1,3] == [0,11,9]
sro_invert :: Z12 -> [Z12] -> [Z12]
sro_invert = Z.z_sro_invert id

-- | Composition of 'invert' about @0@ and 'tn'.
--
-- > tni 4 [1,5,6] == [3,11,10]
-- > (sro_invert 0 . sro_tn  4) [1,5,6] == [7,3,2]
sro_tni :: Z12 -> [Z12] -> [Z12]
sro_tni = Z.z_sro_tni id

-- | Modulo 12 multiplication
--
-- > sro_mn 11 [0,1,4,9] == sro_tni 0 [0,1,4,9]
sro_mn :: Z12 -> [Z12] -> [Z12]
sro_mn = Z.z_sro_mn id

-- | M5, ie. 'mn' @5@.
--
-- > sro_m5 [0,1,3] == [0,5,3]
sro_m5 :: [Z12] -> [Z12]
sro_m5 = sro_mn 5

-- | T-related sequences of /p/.
--
-- > length (sro_t_related [0,3,6,9]) == 12
sro_t_related :: [Z12] -> [[Z12]]
sro_t_related = Z.z_sro_t_related id

-- | T\/I-related sequences of /p/.
--
-- > length (ti_related [0,1,3]) == 24
-- > length (ti_related [0,3,6,9]) == 24
-- > ti_related [0] == map return [0..11]
sro_ti_related :: [Z12] -> [[Z12]]
sro_ti_related = Z.z_sro_ti_related id

-- | R\/T\/I-related sequences of /p/.
--
-- > length (rti_related [0,1,3]) == 48
-- > length (rti_related [0,3,6,9]) == 24
sro_rti_related :: [Z12] -> [[Z12]]
sro_rti_related = Z.z_sro_rti_related id

-- | T\/M\/I-related sequences of /p/, duplicates removed.
sro_tmi_related :: [Z12] -> [[Z12]]
sro_tmi_related p = let q = sro_ti_related p in nub (q ++ map sro_m5 q)

-- | R\/T\/M\/I-related sequences of /p/, duplicates removed.
sro_rtmi_related :: [Z12] -> [[Z12]]
sro_rtmi_related p = let q = sro_tmi_related p in nub (q ++ map reverse q)

-- | r\/R\/T\/M\/I-related sequences of /p/, duplicates removed.
sro_rrtmi_related :: [Z12] -> [[Z12]]
sro_rrtmi_related p = nub (concatMap sro_rtmi_related (T.rotations p))

-- * Sequence operations

-- | Variant of 'tn', transpose /p/ so first element is /n/.
--
-- > sro_tn_to 5 [0,1,3] == [5,6,8]
-- > map (sro_tn_to 0) [[0,1,3],[1,3,0],[3,0,1]] == [[0,1,3],[0,2,11],[0,9,10]]
sro_tn_to :: Z12 -> [Z12] -> [Z12]
sro_tn_to = Z.z_sro_tn_to id

-- | Variant of 'invert', inverse about /n/th element.
--
-- > map (sro_invert_ix 0) [[0,1,3],[3,4,6]] == [[0,11,9],[3,2,0]]
-- > map (sro_invert_ix 1) [[0,1,3],[3,4,6]] == [[2,1,11],[5,4,2]]
sro_invert_ix :: Int -> [Z12] -> [Z12]
sro_invert_ix = Z.z_sro_invert_ix id

-- | The standard t-matrix of /p/.
--
-- > tmatrix [0,1,3] == [[0,1,3]
-- >                    ,[11,0,2]
-- >                    ,[9,10,0]]
tmatrix :: [Z12] -> [[Z12]]
tmatrix = Z.z_tmatrix id
