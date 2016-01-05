-- | List permutation functions.
module Music.Theory.Permutations.List where

import Data.List {- base -}
import qualified Math.Combinatorics.Multiset as C {- multiset-comb -}

import qualified Music.Theory.Permutations as P {- hmt -}

-- | Generate all permutations.
--
-- > permutations [0,3] == [[0,3],[3,0]]
-- > length (permutations [1..5]) == P.n_permutations 5
permutations :: (Eq a) => [a] -> [[a]]
permutations i =
    let f p = P.apply_permutation p i
    in map f (P.permutations_n (length i))

-- | Generate all distinct permutations of a multi-set.
--
-- > multiset_permutations [0,1,1] == [[0,1,1],[1,1,0],[1,0,1]]
multiset_permutations :: (Ord a) => [a] -> [[a]]
multiset_permutations = C.permutations . C.fromList

factorial :: (Enum a, Num a) => a -> a
factorial n = product [1..n]

-- | Calculate number of permutations of a multiset.
--
-- > let r = factorial 11 `div` product (map factorial [1,4,4,2])
-- > in multiset_permutations_n "MISSISSIPPI" == r
--
-- > multiset_permutations_n "MISSISSIPPI" == 34650
-- > length (multiset_permutations "MISSISSIPPI") == 34650
multiset_permutations_n :: Ord a => [a] -> Int
multiset_permutations_n x =
    let occ = map length . group . sort
        n = factorial (length x)
        d = product $ map factorial $ occ x
    in n `div` d
