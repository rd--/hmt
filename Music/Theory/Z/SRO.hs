-- | Serial (ordered) pitch-class operations on 'Z'.
module Music.Theory.Z.SRO where

import Data.List {- base -}

import qualified Music.Theory.List as T
import Music.Theory.Z

-- | Transpose /p/ by /n/.
--
-- > tn 5 4 [0,1,4] == [4,0,3]
-- > tn 12 4 [1,5,6] == [5,9,10]
tn :: (Integral i, Functor f) => i -> i -> f i -> f i
tn z n = fmap (z_add z n)

-- | Invert /p/ about /n/.
--
-- > invert 5 0 [0,1,4] == [0,4,1]
-- > invert 12 6 [4,5,6] == [8,7,6]
-- > invert 12 0 [0,1,3] == [0,11,9]
invert :: (Integral i, Functor f) => i -> i -> f i -> f i
invert z n = fmap (\p -> z_sub z n (z_sub z p  n))

-- | Composition of 'invert' about @0@ and 'tn'.
--
-- > tni 5 1 [0,1,3] == [1,0,3]
-- > tni 12 4 [1,5,6] == [3,11,10]
-- > (invert 12 0 . tn  12 4) [1,5,6] == [7,3,2]
tni :: (Integral i, Functor f) => i -> i -> f i -> f i
tni z n = tn z n . invert z 0

-- | Modulo multiplication.
--
-- > mn 12 11 [0,1,4,9] == tni 12 0 [0,1,4,9]
mn :: (Integral i, Functor f) => i -> i -> f i -> f i
mn z n = fmap (z_mul z n)

-- | T-related sequences of /p/.
--
-- > length (t_related 12 [0,3,6,9]) == 12
t_related :: (Integral i, Functor f) => i -> f i -> [f i]
t_related z p = fmap (\n -> tn z n p) [0..11]

-- | T\/I-related sequences of /p/.
--
-- > length (ti_related 12 [0,1,3]) == 24
-- > length (ti_related 12 [0,3,6,9]) == 24
-- > ti_related 12 [0] == map return [0..11]
ti_related :: (Eq (f i), Integral i, Functor f) => i -> f i -> [f i]
ti_related z p = nub (t_related z p ++ t_related z (invert z 0 p))

-- | R\/T\/I-related sequences of /p/.
--
-- > length (rti_related 12 [0,1,3]) == 48
-- > length (rti_related 12 [0,3,6,9]) == 24
rti_related :: Integral i => i -> [i] -> [[i]]
rti_related z p = let q = ti_related z p in nub (q ++ map reverse q)

-- * Sequence operations

-- | Variant of 'tn', transpose /p/ so first element is /n/.
--
-- > tn_to 12 5 [0,1,3] == [5,6,8]
tn_to :: Integral a => a -> a -> [a] -> [a]
tn_to z n p =
    case p of
      [] -> []
      x:xs -> n : tn z (z_sub z n x) xs

-- | Variant of 'invert', inverse about /n/th element.
--
-- > map (invert_ix 12 0) [[0,1,3],[3,4,6]] == [[0,11,9],[3,2,0]]
-- > map (invert_ix 12 1) [[0,1,3],[3,4,6]] == [[2,1,11],[5,4,2]]
invert_ix :: Integral i => i -> Int -> [i] -> [i]
invert_ix z n p = invert z (p !! n) p

-- | The standard t-matrix of /p/.
--
-- > tmatrix 12 [0,1,3] == [[0,1,3]
-- >                       ,[11,0,2]
-- >                       ,[9,10,0]]
tmatrix :: Integral i => i -> [i] -> [[i]]
tmatrix z p = map (\n -> tn z n p) (tn_to z 0 (invert_ix z 0 p))
