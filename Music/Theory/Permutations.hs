module Music.Theory.Permutations (permutations
                                 ,multiset_permutations) where

import qualified Data.Map as M
import qualified Data.Permute as P
import qualified Math.Combinatorics.Multiset as C

all_ps :: P.Permute -> [P.Permute]
all_ps p =
    let r = P.next p
    in maybe [p] (\np -> p : all_ps np) r

n_ps :: Int -> [[Int]]
n_ps n =
    let p = P.permute n
        ps = all_ps p
    in map P.elems ps

-- Generate list of all permutations.
permutations :: [a] -> [[a]]
permutations xs =
    let m = M.fromList (zip [0..] xs)
        ps = n_ps (M.size m)
        r = map (\i -> M.findWithDefault (error "permutations") i m)
    in map r ps

-- Generate list of all distinct permutations.
multiset_permutations :: (Ord a) => [a] -> [[a]]
multiset_permutations = C.permutations . C.fromList
