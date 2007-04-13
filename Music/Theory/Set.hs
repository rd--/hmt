module Music.Theory.Set (powerset, permutations, dyads) where

import Control.Monad

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

permutations :: [a] -> [[a]]
permutations []     = [[]]
permutations (x:xs) = [ zs | ys <- permutations xs , zs <- everywhere x ys ]

everywhere :: a -> [a] -> [[a]]
everywhere x []     = [[x]]
everywhere x (y:ys) = (x:y:ys) : [ y:zs | zs <- everywhere x ys ]

dyads :: [a] -> [(a,a)]
dyads [] = []
dyads (x:xs) = dyads xs ++ [ (x,y) | y <- xs ]
