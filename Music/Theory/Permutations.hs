-- | Permutation functions.
module Music.Theory.Permutations where

import Data.List {- base -}
import qualified Numeric {- base -}

import qualified Music.Theory.List as L {- hmt -}

-- | Factorial function.
--
-- > (factorial 20,maxBound::Int)
factorial :: Integral n => n -> n
factorial n = product [1..n]

-- | Number of /k/ element permutations of a set of /n/ elements.
--
-- > let f = nk_permutations in (f 3 2,f 3 3,f 4 3,f 4 4,f 13 3,f 12 12) == (6,6,24,24,1716,479001600)
nk_permutations :: Integral a => a -> a -> a
nk_permutations n k = factorial n  `div` factorial (n - k)

-- | Number of /nk/ permutations where /n/ '==' /k/.
--
-- > map n_permutations [1..8] == [1,2,6,24,120,720,5040,40320]
-- > n_permutations 12 == 479001600
-- > n_permutations 16 `div` 1000000 == 20922789
n_permutations :: (Integral a) => a -> a
n_permutations n = nk_permutations n n

-- | Permutation given as a zero-indexed list of destination indices.
type Permutation = [Int]

{- | Generate the permutation from /p/ to /q/, ie. the permutation
     that, when applied to /p/, gives /q/.

> p = permutation "abc" "bac"
> p == [1,0,2]
> apply_permutation p "abc" == "bac"
-}
permutation :: Eq t => [t] -> [t] -> Permutation
permutation p q =
    let f x = L.elem_index_unique x p
    in map f q

-- | Permutation to list of swaps, ie. 'zip' [0..]
--
-- > permutation_to_swaps [0,2,1,3] == [(0,0),(1,2),(2,1),(3,3)]
permutation_to_swaps :: Permutation -> [(Int,Int)]
permutation_to_swaps = zip [0..]

-- | Inverse of 'permutation_to_swaps', ie. 'map' 'snd' '.' 'sort'
swaps_to_permutation :: [(Int,Int)] -> Permutation
swaps_to_permutation = map snd . sort

-- | List of cycles to list of swaps.
--
-- > cycles_to_swaps [[0,2],[1],[3,4]] == [(0,2),(1,1),(2,0),(3,4),(4,3)]
cycles_to_swaps :: [[Int]] -> [(Int,Int)]
cycles_to_swaps = sort . concatMap (L.adj2_cyclic 1)

-- > swaps_to_cycles [(0,2),(1,1),(2,0),(3,4),(4,3)] == [[0,2],[1],[3,4]]
swaps_to_cycles :: [(Int, Int)] -> [[Int]]
swaps_to_cycles s =
  let z = length s
      next k = L.lookup_err k s
      trace k =
        let f r i = let j = next i in if j == k then reverse r else f (j : r) j
        in f [k] k
      step r k =
        if k == z
        then reverse r
        else if k `elem` concat r then step r (k + 1) else step (trace k : r) (k + 1)
  in step [] 0

{- | Apply permutation /f/ to /p/.

> let p = permutation [1..4] [4,3,2,1]
> p == [3,2,1,0]
> apply_permutation p [1..4] == [4,3,2,1]
-}
apply_permutation :: Permutation -> [t] -> [t]
apply_permutation f p = map (p !!) f

-- | Composition of 'apply_permutation' and 'from_cycles_zero_indexed'.
--
-- > apply_permutation_c_zero_indexed [[0,3],[1,2]] [1..4] == [4,3,2,1]
-- > apply_permutation_c_zero_indexed [[0,2],[1],[3,4]] [1..5] == [3,2,1,5,4]
-- > apply_permutation_c_zero_indexed [[0,1,4],[2,3]] [1..5] == [2,5,4,3,1]
-- > apply_permutation_c_zero_indexed [[0,1,3],[2,4]] [1..5] == [2,4,5,1,3]
apply_permutation_c_zero_indexed :: [[Int]] -> [a] -> [a]
apply_permutation_c_zero_indexed = apply_permutation . from_cycles_zero_indexed

-- > p_inverse [2,7,4,9,8,3,5,0,6,1] == [7,9,0,5,2,6,8,1,4,3]
p_inverse :: Permutation -> Permutation
p_inverse = map snd . sort . flip zip [0..]

p_cycles :: Permutation -> [[Int]]
p_cycles = swaps_to_cycles . permutation_to_swaps

{- | True if the inverse of /p/ is /p/.

> non_invertible [1,0,2] == True
> non_invertible [2,7,4,9,8,3,5,0,6,1] == False

> let p = permutation [1..4] [4,3,2,1]
> non_invertible p == True && p_cycles p == [[0,3],[1,2]]
-}
non_invertible :: Permutation -> Bool
non_invertible p = p == p_inverse p

-- | Generate a permutation from the cycles /c/ (zero-indexed)
--
-- > apply_permutation (from_cycles_zero_indexed [[0,1,2,3]]) [1..4] == [2,3,4,1]
from_cycles_zero_indexed :: [[Int]] -> Permutation
from_cycles_zero_indexed = swaps_to_permutation . cycles_to_swaps

from_cycles_one_indexed :: [[Int]] -> Permutation
from_cycles_one_indexed = from_cycles_zero_indexed . map (map (subtract 1))

-- | Generate all permutations of size /n/ (naive)
--
-- > let r = [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
-- > map one_line (permutations_n 3) == r
permutations_n :: Int -> [Permutation]
permutations_n n =
  let minus [] _ = []
      minus (x:xs) i = if x < i then x : minus xs i else xs
      f [] = [[]]
      f xs = [i : ys | i <- xs , ys <- f (xs `minus` i)]
  in case n of
       0 -> []
       1 -> [[0]]
       _ -> f [0 .. n - 1]

p_size :: Permutation -> Int
p_size = length

{- | Composition of /q/ then /p/.

> let p = from_cycles_zero_indexed [[0,2],[1],[3,4]]
> let q = from_cycles_zero_indexed [[0,1,4],[2,3]]
> let r = p `compose` q
> apply_permutation r [1,2,3,4,5] == [2,4,5,1,3]
-}
compose :: Permutation -> Permutation -> Permutation
compose p q =
    let n = p_size q
        i = [1 .. n]
        j = apply_permutation p i
        k = apply_permutation q j
    in permutation i k

-- | One-indexed 'p_cycles'
cycles_one_indexed :: Permutation -> [[Int]]
cycles_one_indexed = map (map (+ 1)) . p_cycles

{- | 'flip' of 'compose'

> cycles_one_indexed (from_cycles_one_indexed [[1,5],[2,3,6],[4]] `permutation_mul` from_cycles_one_indexed [[1,6,4],[2],[3,5]])
-}
permutation_mul :: Permutation -> Permutation -> Permutation
permutation_mul p q = compose q p

-- | Two line notation of /p/.
--
-- > two_line (permutation [0,1,3] [1,0,3]) == ([1,2,3],[2,1,3])
two_line :: Permutation -> ([Int],[Int])
two_line p =
    let n = p_size p
        i = [1..n]
    in (i,apply_permutation p i)

-- | One line notation of /p/.
--
-- > one_line (permutation [0,1,3] [1,0,3]) == [2,1,3]
--
-- > let r = [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
-- > map one_line (permutations_n 3) == r
one_line :: Permutation -> [Int]
one_line = snd . two_line

-- | Variant of 'one_line' that produces a compact string.
--
-- > one_line_compact (permutation [0,1,3] [1,0,3]) == "213"
--
-- > let p = permutations_n 3
-- > unwords (map one_line_compact p) == "123 132 213 231 312 321"
one_line_compact :: Permutation -> String
one_line_compact =
    let f n = if n >= 0 && n <= 15
              then Numeric.showHex n ""
              else error "one_line_compact:not(0-15)"
    in concatMap f . one_line

-- | Multiplication table of symmetric group /n/.
--
-- > unlines (map (unwords . map one_line_compact) (multiplication_table 3))
--
-- @
-- ==> 123 132 213 231 312 321
--     132 123 312 321 213 231
--     213 231 123 132 321 312
--     231 213 321 312 123 132
--     312 321 132 123 231 213
--     321 312 231 213 132 123
-- @
multiplication_table :: Int -> [[Permutation]]
multiplication_table n =
    let ps = permutations_n n
        f p = map (compose p) ps
    in map f ps

{-

let q = permutation [1..4] [2,3,4,1] -- [[0,1,2,3]]
(q,non_invertible q,p_cycles q,apply_permutation q [1..4])

let p = permutation [1..5] [3,2,1,5,4] -- [[0,2],[1],[3,4]]
let q = permutation [1..5] [2,5,4,3,1] -- [[0,1,4],[2,3]]
let r = permutation [1..5] [2,4,5,1,3] -- [[0,1,3],[2,4]]
(non_invertible p,p_cycles p,apply_permutation p [1..5])
(non_invertible q,p_cycles q,apply_permutation q [1..5])
(non_invertible r,p_cycles r,apply_permutation r [1..5])

map p_cycles (permutations_n 3)
map p_cycles (permutations_n 4)

import Data.List {- base -}
partition not (map non_invertible (permutations_n 4))
putStrLn $ unlines $ map unwords $ permutations ["A0","A1","B0"]

-}
