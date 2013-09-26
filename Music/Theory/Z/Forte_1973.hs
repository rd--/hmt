-- | Allen Forte. /The Structure of Atonal Music/. Yale University
-- Press, New Haven, 1973.
module Music.Theory.Z.Forte_1973 where

import Data.List {- base -}
import Data.Maybe {- base -}

import Music.Theory.List
import qualified Music.Theory.Set.List as S
import Music.Theory.Z
import Music.Theory.Z.SRO

-- * Prime form

-- | T-related rotations of /p/.
--
-- > t_rotations 12 [0,1,3] == [[0,1,3],[0,2,11],[0,9,10]]
t_rotations :: Integral a => a -> [a] -> [[a]]
t_rotations z p =
    let r = rotations (sort p)
    in map (tn_to z 0) r

-- | T\/I-related rotations of /p/.
--
-- > ti_rotations 12 [0,1,3] == [[0,1,3],[0,2,11],[0,9,10]
-- >                            ,[0,9,11],[0,2,3],[0,1,10]]
ti_rotations :: Integral a => a -> [a] -> [[a]]
ti_rotations z p =
    let q = invert z 0 p
        r = rotations (sort p) ++ rotations (sort q)
    in map (tn_to z 0) r

-- | Variant with default value for empty input list case.
minimumBy_or :: a -> (a -> a -> Ordering) -> [a] -> a
minimumBy_or p f q = if null q then p else minimumBy f q

-- | Prime form rule requiring comparator, considering 't_rotations'.
t_cmp_prime :: Integral a => a -> ([a] -> [a] -> Ordering) -> [a] -> [a]
t_cmp_prime z f = minimumBy_or [] f . t_rotations z

-- | Prime form rule requiring comparator, considering 'ti_rotations'.
ti_cmp_prime :: Integral a => a -> ([a] -> [a] -> Ordering) -> [a] -> [a]
ti_cmp_prime z f = minimumBy_or [] f . ti_rotations z

-- | Forte comparison function (rightmost first then leftmost outwards).
--
-- > forte_cmp [0,1,3,6,8,9] [0,2,3,6,7,9] == LT
forte_cmp :: (Ord t) => [t] -> [t] -> Ordering
forte_cmp [] [] = EQ
forte_cmp p  q  =
    let r = compare (last p) (last q)
    in if r == EQ then compare p q else r

-- | Forte prime form, ie. 'cmp_prime' of 'forte_cmp'.
--
-- > forte_prime 12 [0,1,3,6,8,9] == [0,1,3,6,8,9]
-- > forte_prime 5 [0,1,4] == [0,1,2]
--
-- > S.set (map (forte_prime 5) (S.powerset [0..4]))
forte_prime :: Integral a => a -> [a] -> [a]
forte_prime z = ti_cmp_prime z forte_cmp

-- * ICV Metric

-- | Interval class of i interval /i/.
--
-- > map (ic 5) [1,2,3,4] == [1,2,2,1]
-- > map (ic 12) [5,6,7] == [5,6,5]
-- > map (ic 12 . to_Z 12) [-13,-1,0,1,13] == [1,1,0,1,1]
ic :: Integral a => a -> a -> a
ic z i = if i <= (z `div` 2) then i else z_sub z z i

-- | Forte notation for interval class vector.
--
-- > icv 12 [0,1,2,4,7,8] == [3,2,2,3,3,2]
icv :: (Integral i, Num n) => i -> [i] -> [n]
icv z s =
    let i = map (ic z . uncurry (z_sub z)) (S.pairs s)
        j = map f (group (sort i))
        k = map (`lookup` j) [1 .. z `div` 2]
        f l = (head l,genericLength l)
    in map (fromMaybe 0) k

-- * BIP Metric

-- | Basic interval pattern, see Allen Forte \"The Basic Interval Patterns\"
-- /JMT/ 17/2 (1973):234-272
--
-- >>> bip 0t95728e3416
-- 11223344556
--
-- > bip 12 [0,10,9,5,7,2,8,11,3,4,1,6] == [1,1,2,2,3,3,4,4,5,5,6]
bip :: Integral a => a -> [a] -> [a]
bip z = sort . map (ic z . to_Z z) . d_dx
