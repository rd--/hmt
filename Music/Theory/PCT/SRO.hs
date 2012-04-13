-- | Serial pitch-class operations on 'Z12'.
module Music.Theory.PCT.SRO where

import Data.List
import qualified Music.Theory.List as L
import Music.Theory.Z12

-- * Pitch class operations

-- | Transpose by n.
--
-- >>> sro T4 156
-- 59A
--
-- > tn 4 [1,5,6] == [5,9,10]
tn :: Z12 -> [Z12] -> [Z12]
tn n = map (+ n)

-- | Transpose so first element is n.
--
-- > transposeTo 5 [0,1,3] == [5,6,8]
transposeTo :: Z12 -> [Z12] -> [Z12]
transposeTo n p =
    case p of
      [] -> []
      x:xs -> n : tn (n - x) xs

-- | All transpositions.
transpositions :: [Z12] -> [[Z12]]
transpositions p = map (`tn` p) [0..11]

-- | Invert about n.
--
-- > invert 6 [4,5,6] == [8,7,6]
-- > invert 0 [0,1,3] == [0,11,9]
invert :: Z12 -> [Z12] -> [Z12]
invert n = map (\p -> n - (p - n))

-- | Invert about first element.
--
-- > map invertSelf [[0,1,3],[3,4,6]] == [[0,11,9],[3,2,0]]
invertSelf :: [Z12] -> [Z12]
invertSelf p =
    case p of
      [] -> []
      x:xs -> invert x (x:xs)

-- | Composition of 'invert' about @0@ and 'tn'.
--
-- >>> sro T4I 156
-- 3BA
--
-- > tni 4 [1,5,6] == [3,11,10]
--
-- >>> echo 156 | sro T4  | sro T0I
-- 732
--
-- > (invert 0 . tn  4) [1,5,6] == [7,3,2]
tni :: Z12 -> [Z12] -> [Z12]
tni n = tn n . invert 0

-- | Modulo 12 multiplication
--
-- > mn 11 [0,1,4,9] == tni 0 [0,1,4,9]
mn :: Z12 -> [Z12] -> [Z12]
mn n = map (* n)

-- | M5, ie. 'mn' @5@.
--
-- > m5 [0,1,3] == [0,5,3]
m5 :: [Z12] -> [Z12]
m5 = mn 5

-- | Set of all tranpositions.
--
-- > length (all_Tn [0,1,3]) == 12
all_Tn :: [Z12] -> [[Z12]]
all_Tn p = map (`tn` p) [0..11]

-- | Set of all tranpositions and inversions.
--
-- > length (all_TnI [0,1,3]) == 24
all_TnI :: [Z12] -> [[Z12]]
all_TnI p =
    let ps = all_Tn p
    in ps ++ map (invert 0) ps

-- | Set of all retrogrades,tranpositions and inversions.
--
-- > length (all_RTnI [0,1,3]) == 48
all_RTnI :: [Z12] -> [[Z12]]
all_RTnI p =
    let ps = all_TnI p
    in ps ++ map reverse ps

-- | Set of all rotations and retrogrades.
--
-- > map (length . all_rR) [[0,1,3],[0,1,3,6]] == [6,8]
all_rR :: [Z12] -> [[Z12]]
all_rR p = L.rotations p ++ L.rotations (reverse p)

-- | Set of all rotations,retrogrades,tranpositions and inversions.
--
-- > length (all_rRTnI [0,1,3]) == 192
all_rRTnI :: [Z12] -> [[Z12]]
all_rRTnI p =
    let ps = all_RTnI p
    in ps ++ concatMap L.rotations ps

-- | Set of all tranpositions,@M5@ and inversions.
all_TnMI :: [Z12] -> [[Z12]]
all_TnMI p =
    let ps = all_TnI p
    in ps ++ map m5 ps

-- | Set of all retrogrades,tranpositions,@M5@ and inversions.
all_RTnMI :: [Z12] -> [[Z12]]
all_RTnMI p =
    let ps = all_TnMI p
    in ps ++ map reverse ps

-- | Set of all rotations,retrogrades,tranpositions,@M5@ and inversions.
all_rRTnMI :: [Z12] -> [[Z12]]
all_rRTnMI = map snd . sros

-- * Serial operations

-- | Serial Operator,of the form rRTMI.
data SRO = SRO Z12 Bool Z12 Bool Bool
           deriving (Eq,Show)

-- | Serial operation.
--
-- >>> sro T4 156
-- 59A
--
-- > sro (rnrtnmi "T4") (pco "156") == [5,9,10]
--
-- >>> echo 024579 | sro RT4I
-- 79B024
--
-- > sro (SRO 0 True 4 False True) [0,2,4,5,7,9] == [7,9,11,0,2,4]
--
-- >>> sro T4I 156
-- 3BA
--
-- > sro (rnrtnmi "T4I") (pco "156") == [3,11,10]
-- > sro (SRO 0 False 4 False True) [1,5,6] == [3,11,10]
--
-- >>> echo 156 | sro T4  | sro T0I
-- 732
--
-- > (sro (rnrtnmi "T0I") . sro (rnrtnmi "T4")) (pco "156") == [7,3,2]
--
-- >>> echo 024579 | sro RT4I
-- 79B024
--
-- > sro (rnrtnmi "RT4I") (pco "024579") == [7,9,11,0,2,4]
--
-- > sro (SRO 1 True 1 True False) [0,1,2,3] == [11,6,1,4]
-- > sro (SRO 1 False 4 True True) [0,1,2,3] == [11,6,1,4]
sro :: SRO -> [Z12] -> [Z12]
sro (SRO r r' t m i) x =
    let x1 = if i then invert 0 x else x
        x2 = if m then m5 x1 else x1
        x3 = tn t x2
        x4 = if r' then reverse x3 else x3
    in L.genericRotate_left r x4

-- | The total set of serial operations.
sros :: [Z12] -> [(SRO,[Z12])]
sros x = [let o = (SRO r r' t m i) in (o,sro o x) |
          r <- [0 .. genericLength x - 1],
          r' <- [False,True],
          t <- [0 .. 11],
          m <- [False,True],
          i <- [False,True]]

-- | The set of transposition 'SRO's.
sro_Tn ::[SRO]
sro_Tn = [SRO 0 False n False False | n <- [0..11]]

-- | The set of transposition and inversion 'SRO's.
sro_TnI ::[SRO]
sro_TnI = [SRO 0 False n False i |
           n <- [0..11],
           i <- [False,True]]

-- | The set of retrograde and transposition and inversion 'SRO's.
sro_RTnI ::[SRO]
sro_RTnI = [SRO 0 r n False i |
            r <- [True,False],
            n <- [0..11],
            i <- [False,True]]

-- | The set of transposition,@M5@ and inversion 'SRO's.
sro_TnMI ::[SRO]
sro_TnMI = [SRO 0 False n m i |
            n <- [0..11],
            m <- [True,False],
            i <- [True,False]]

-- | The set of retrograde,transposition,@M5@ and inversion 'SRO's.
sro_RTnMI ::[SRO]
sro_RTnMI = [SRO 0 r n m i |
             r <- [True,False],
             n <- [0..11],
             m <- [True,False],
             i <- [True,False]]

-- * Set operations.

-- | Map to pitch-class and reduce to set.
--
-- > pcset [1,13] == [1]
pcset :: (Integral a) => [a] -> [Z12]
pcset = nub . sort . map fromIntegral

-- * Sequence operations

-- | The standard t-matrix of /p/.
--
-- > tmatrix [0,1,3] == [[0,1,3]
-- >                    ,[11,0,2]
-- >                    ,[9,10,0]]
tmatrix :: [Z12] -> [[Z12]]
tmatrix p = map (`tn` p) (transposeTo 0 (invertSelf p))
