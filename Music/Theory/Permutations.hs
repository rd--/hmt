-- | Permutation functions.
module Music.Theory.Permutations where

import qualified Data.Permute as P {- permutation -}
import qualified Numeric {- base -}

import qualified Music.Theory.List as L {- hmt -}

-- | Factorial function.
--
-- > (factorial 13,maxBound::Int)
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

-- | Generate the permutation from /p/ to /q/, ie. the permutation
-- that, when applied to /p/, gives /q/.
--
-- > apply_permutation (permutation "abc" "bac") "abc" == "bac"
permutation :: (Eq a) => [a] -> [a] -> P.Permute
permutation p q =
    let n = length p
        f x = L.elem_index_unique x p
    in P.listPermute n (map f q)

-- | Apply permutation /f/ to /p/.
--
-- > let p = permutation [1..4] [4,3,2,1]
-- > apply_permutation p [1..4] == [4,3,2,1]
apply_permutation :: P.Permute -> [a] -> [a]
apply_permutation f p = map (p !!) (P.elems f)

-- | Composition of 'apply_permutation' and 'from_cycles'.
--
-- > apply_permutation_c [[0,3],[1,2]] [1..4] == [4,3,2,1]
-- > apply_permutation_c [[0,2],[1],[3,4]] [1..5] == [3,2,1,5,4]
-- > apply_permutation_c [[0,1,4],[2,3]] [1..5] == [2,5,4,3,1]
-- > apply_permutation_c [[0,1,3],[2,4]] [1..5] == [2,4,5,1,3]
apply_permutation_c :: [[Int]] -> [a] -> [a]
apply_permutation_c = apply_permutation . from_cycles

-- | True if the inverse of /p/ is /p/.
--
-- > non_invertible (permutation [0,1,3] [1,0,3]) == True
--
-- > let p = permutation [1..4] [4,3,2,1]
-- > non_invertible p == True && P.cycles p == [[0,3],[1,2]]
non_invertible :: P.Permute -> Bool
non_invertible p = p == P.inverse p

-- | Generate a permutation from the cycles /c/.
--
-- > apply_permutation (from_cycles [[0,1,2,3]]) [1..4] == [2,3,4,1]
from_cycles :: [[Int]] -> P.Permute
from_cycles c = P.cyclesPermute (sum (map length c)) c

-- | Generate all permutations of size /n/.
--
-- > let r = [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
-- > map one_line (permutations_n 3) == r
permutations_n :: Int -> [P.Permute]
permutations_n n =
    let f p = let r = P.next p
              in maybe [p] (\np -> p : f np) r
    in f (P.permute n)

-- | Composition of /q/ then /p/.
--
-- > let p = from_cycles [[0,2],[1],[3,4]]
-- > let q = from_cycles [[0,1,4],[2,3]]
-- > let r = p `compose` q
-- > apply_permutation r [1,2,3,4,5] == [2,4,5,1,3]
compose :: P.Permute -> P.Permute -> P.Permute
compose p q =
    let n = P.size q
        i = [1 .. n]
        j = apply_permutation p i
        k = apply_permutation q j
    in permutation i k

-- | Two line notation of /p/.
--
-- > two_line (permutation [0,1,3] [1,0,3]) == ([1,2,3],[2,1,3])
two_line :: P.Permute -> ([Int],[Int])
two_line p =
    let n = P.size p
        i = [1..n]
    in (i,apply_permutation p i)

-- | One line notation of /p/.
--
-- > one_line (permutation [0,1,3] [1,0,3]) == [2,1,3]
--
-- > let r = [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
-- > map one_line (permutations_n 3) == r
one_line :: P.Permute -> [Int]
one_line = snd . two_line

-- | Variant of 'one_line' that produces a compact string.
--
-- > one_line_compact (permutation [0,1,3] [1,0,3]) == "213"
--
-- > let p = permutations_n 3
-- > unwords (map one_line_compact p) == "123 132 213 231 312 321"
one_line_compact :: P.Permute -> String
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
multiplication_table :: Int -> [[P.Permute]]
multiplication_table n =
    let ps = permutations_n n
        f p = map (compose p) ps
    in map f ps

{-

let q = permutation [1..4] [2,3,4,1] -- [[0,1,2,3]]
(q,non_invertible q,P.cycles q,apply_permutation q [1..4])

let p = permutation [1..5] [3,2,1,5,4] -- [[0,2],[1],[3,4]]
let q = permutation [1..5] [2,5,4,3,1] -- [[0,1,4],[2,3]]
let r = permutation [1..5] [2,4,5,1,3] -- [[0,1,3],[2,4]]
(non_invertible p,P.cycles p,apply_permutation p [1..5])
(non_invertible q,P.cycles q,apply_permutation q [1..5])
(non_invertible r,P.cycles r,apply_permutation r [1..5])

map P.cycles (permutations_n 3)
map P.cycles (permutations_n 4)

import Data.List {- base -}
partition not (map non_invertible (permutations_n 4))
putStrLn $ unlines $ map unwords $ permutations ["A0","A1","B0"]

-}
