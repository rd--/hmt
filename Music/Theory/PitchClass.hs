-- | Pitch class operations on integers.
module Music.Theory.PitchClass where

import Data.Maybe
import Data.List
import Music.Theory.List
import Music.Theory.Set

-- * Pitch class operations

-- | Modulo twelve.
--
-- > map mod12 [11,12,-1] == [11,0,11]
mod12 :: (Integral a) => a -> a
mod12 = (`mod` 12)

-- | Pitch class, synonym for 'mod12'.
pc :: (Integral a) => a -> a
pc = mod12

-- | Map to pitch-class and reduce to set.
--
-- > pcset [1,13] == [1]
pcset :: (Integral a) => [a] -> [a]
pcset = set . map pc

-- | Transpose by n.
--
-- >>> sro T4 156
-- 59A
--
-- > tn 4 [1,5,6] == [5,9,10]
tn :: (Integral a) => a -> [a] -> [a]
tn n = map (pc . (+ n))

-- | Transpose so first element is n.
--
-- > transposeTo 5 [0,1,3] == [5,6,8]
transposeTo :: (Integral a) => a -> [a] -> [a]
transposeTo n p =
    case p of
      [] -> []
      x:xs -> n : tn (n - x) xs

-- | All transpositions.
transpositions :: (Integral a) => [a] -> [[a]]
transpositions p = map (`tn` p) [0..11]

-- | Invert about n.
--
-- > invert 6 [4,5,6] == [8,7,6]
-- > invert 0 [0,1,3] == [0,11,9]
invert :: (Integral a) => a -> [a] -> [a]
invert n = map (pc . (\p -> n - (p - n)))

-- | Invert about first element.
--
-- > map invertSelf [[0,1,3],[3,4,6]] == [[0,11,9],[3,2,0]]
invertSelf :: (Integral a) => [a] -> [a]
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
tni :: (Integral a) => a -> [a] -> [a]
tni n = tn n . invert 0

-- | Rotate left by /n/ 'mod' /#p/ places.
--
-- > rotate 8 [1..5] == [4,5,1,2,3]
rotate :: (Integral n) => n -> [a] -> [a]
rotate n p =
    let m = n `mod` genericLength p
    in genericRotate_left m p

-- | Rotate right by /n/ places.
--
-- > rotate_r 8 [1..5] == [3,4,5,1,2]
rotate_r :: (Integral n) => n -> [a] -> [a]
rotate_r = rotate . negate

-- | All rotations.
--
-- > rotations [0,1,3] == [[0,1,3],[1,3,0],[3,0,1]]
rotations :: [a] -> [[a]]
rotations p = map (`rotate_left` p) [0 .. length p - 1]

-- | Modulo 12 multiplication
--
-- > mn 11 [0,1,4,9] == tni 0 [0,1,4,9]
mn :: (Integral a) => a -> [a] -> [a]
mn n = map (pc . (* n))

-- | M5, ie. 'mn' @5@.
--
-- > m5 [0,1,3] == [0,5,3]
m5 :: (Integral a) => [a] -> [a]
m5 = mn 5

-- | Set of all tranpositions.
--
-- > length (all_Tn [0,1,3]) == 12
all_Tn :: (Integral a) => [a] -> [[a]]
all_Tn p = map (`tn` p) [0..11]

-- | Set of all tranpositions and inversions.
--
-- > length (all_TnI [0,1,3]) == 24
all_TnI :: (Integral a) => [a] -> [[a]]
all_TnI p =
    let ps = all_Tn p
    in ps ++ map (invert 0) ps

-- | Set of all retrogrades, tranpositions and inversions.
--
-- > length (all_RTnI [0,1,3]) == 48
all_RTnI :: (Integral a) => [a] -> [[a]]
all_RTnI p =
    let ps = all_TnI p
    in ps ++ map reverse ps

-- | Set of all rotations and retrogrades.
--
-- > map (length . all_rR) [[0,1,3],[0,1,3,6]] == [6,8]
all_rR :: (Integral a) => [a] -> [[a]]
all_rR p = rotations p ++ rotations (reverse p)

-- | Set of all rotations, retrogrades, tranpositions and inversions.
--
-- > length (all_rRTnI [0,1,3]) == 192
all_rRTnI :: (Integral a) => [a] -> [[a]]
all_rRTnI p =
    let ps = all_RTnI p
    in ps ++ concatMap rotations ps

-- | Set of all tranpositions, @M5@ and inversions.
all_TnMI :: (Integral a) => [a] -> [[a]]
all_TnMI p =
    let ps = all_TnI p
    in ps ++ map m5 ps

-- | Set of all retrogrades, tranpositions, @M5@ and inversions.
all_RTnMI :: (Integral a) => [a] -> [[a]]
all_RTnMI p =
    let ps = all_TnMI p
    in ps ++ map reverse ps

-- | Set of all rotations, retrogrades, tranpositions, @M5@ and inversions.
all_rRTnMI :: (Integral a) => [a] -> [[a]]
all_rRTnMI = map snd . sros

-- * Serial operations

-- | Serial Operator, of the form rRTMI.
data SRO a = SRO a Bool a Bool Bool
             deriving (Eq, Show)

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
sro :: (Integral a) => SRO a -> [a] -> [a]
sro (SRO r r' t m i) x =
    let x1 = if i then invert 0 x else x
        x2 = if m then m5 x1 else x1
        x3 = tn t x2
        x4 = if r' then reverse x3 else x3
    in rotate r x4

-- | The total set of serial operations.
sros :: (Integral a) => [a] -> [(SRO a, [a])]
sros x = [ let o = (SRO r r' t m i) in (o, sro o x) |
           r <- [0 .. genericLength x - 1],
           r' <- [False, True],
           t <- [0 .. 11],
           m <- [False, True],
           i <- [False, True] ]

-- | The set of transposition 'SRO's.
sro_Tn :: (Integral a) => [SRO a]
sro_Tn = [ SRO 0 False n False False |
           n <- [0..11] ]

-- | The set of transposition and inversion 'SRO's.
sro_TnI :: (Integral a) => [SRO a]
sro_TnI = [ SRO 0 False n False i |
            n <- [0..11],
            i <- [False, True] ]

-- | The set of retrograde and transposition and inversion 'SRO's.
sro_RTnI :: (Integral a) => [SRO a]
sro_RTnI = [ SRO 0 r n False i |
             r <- [True, False],
             n <- [0..11],
             i <- [False, True] ]

-- | The set of transposition, @M5@ and inversion 'SRO's.
sro_TnMI :: (Integral a) => [SRO a]
sro_TnMI = [ SRO 0 False n m i |
             n <- [0..11],
             m <- [True, False],
             i <- [True, False] ]

-- | The set of retrograde, transposition, @M5@ and inversion 'SRO's.
sro_RTnMI :: (Integral a) => [SRO a]
sro_RTnMI = [ SRO 0 r n m i |
              r <- [True, False],
              n <- [0..11],
              m <- [True, False],
              i <- [True, False] ]

-- * Interval operations

-- | Intervals to values, zero is /n/.
--
-- > dx_d 5 [1,2,3] == [5,6,8,11]
dx_d :: (Num a) => a -> [a] -> [a]
dx_d = scanl (+)

-- | Integrate, ie. pitch class segment to interval sequence.
--
-- > d_dx [5,6,8,11] == [1,2,3]
d_dx :: (Num a) => [a] -> [a]
d_dx l =
    case l of
      x:xs -> zipWith (-) xs (x:xs)
      _ -> []

-- | Morris @INT@ operator.
--
-- > int [0,1,3,6,10] == [1,2,3,4]
int :: (Integral a) => [a] -> [a]
int = map mod12 . d_dx

-- | Interval class.
--
-- > map ic [5,6,7] == [5,6,5]
ic :: (Integral a) => a -> a
ic i =
    let i' = mod12 i
    in if i' <= 6 then i' else 12 - i'

-- | Interval class vector.
--
-- > icv [0,1,2,4,7,8] == [3,2,2,3,3,2]
icv :: (Integral a) => [a] -> [a]
icv s =
    let i = map (ic . uncurry (-)) (dyads s)
        j = map f (group (sort i))
        k = map (`lookup` j) [1..6]
        f l = (head l, genericLength l)
    in map (fromMaybe 0) k

-- * Set operations.

-- | Elements of /p/ not in /q/.
--
-- > [1,2,3] `difference` [1,2] == [3]
difference :: (Eq a) => [a] -> [a] -> [a]
difference p q =
    let f e = e `notElem` q
    in filter f p

-- | Pitch classes not in set, ie. 'difference' @[0..11]@.
--
-- > complement [0,2,4,5,7,9,11] == [1,3,6,8,10]
complement :: (Integral a) => [a] -> [a]
complement = difference [0..11]

-- | Is /p/ a subset of /q/, ie. is 'intersect' of /p/ and /q/ '==' /p/.
--
-- > is_subset [1,2] [1,2,3] == True
is_subset :: Eq a => [a] -> [a] -> Bool
is_subset p q = p `intersect` q == p

-- | Is /p/ a superset of /q/, ie. 'flip' 'is_subset'.
--
-- > is_superset [1,2,3] [1,2] == True
is_superset :: Eq a => [a] -> [a] -> Bool
is_superset = flip is_subset

-- * Sequence operations

-- | Is /p/ a subsequence of /q/, ie. synonym for 'isInfixOf'.
--
-- > subsequence [1,2] [1,2,3] == True
subsequence :: (Eq a) => [a] -> [a] -> Bool
subsequence = isInfixOf

-- | The standard t-matrix of /p/.
--
-- > tmatrix [0,1,3] == [[ 0, 1, 3]
-- >                    ,[11, 0, 2]
-- >                    ,[ 9,10, 0]]
tmatrix :: (Integral a) => [a] -> [[a]]
tmatrix p = map (`tn` p) (transposeTo 0 (invertSelf p))
