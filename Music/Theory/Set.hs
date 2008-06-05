module Music.Theory.Set where

import Control.Monad
import Data.List

-- | Remove duplicate elements and sort.
set :: (Ord a) => [a] -> [a]
set = sort . nub

-- | Powerset, ie. set of all all subsets.
powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

-- | Permutations.
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = [ zs | ys <- permutations xs , zs <- everywhere x ys ]
    
everywhere :: a -> [a] -> [[a]]
everywhere x [] = [[x]]
everywhere x (y:ys) = (x:y:ys) : [ y:zs | zs <- everywhere x ys ]

-- | Two element subsets (cf [2] . powerset).
dyads :: [a] -> [(a,a)]
dyads [] = []
dyads (x:xs) = dyads xs ++ [ (x,y) | y <- xs ]

-- | Set expansion
se :: (Ord a) => Int -> [a] -> [[a]]
se n xs = if length xs == n 
          then [xs] 
          else nub (concatMap (se n) [sort (y : xs) | y <- xs])
