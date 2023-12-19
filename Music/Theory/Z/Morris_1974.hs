{- | Robert Morris and D. Starr. \"The Structure of All-Interval Series\".
/Journal of Music Theory/, 18:364-389, 1974.
-}
module Music.Theory.Z.Morris_1974 where

import Control.Monad {- base -}

import qualified Music.Theory.List as List {- hmt-base -}

import qualified Control.Monad.Logic as L {- logict -}

{- | 'msum' '.' 'map' 'return'.

> L.observeAll (fromList [1..7]) == [1..7]
-}
fromList :: MonadPlus m => [a] -> m a
fromList = msum . map return

{- | Interval from /i/ to /j/ in modulo-/n/.

> let f = int_n 12 in (f 0 11,f 11 0) == (11,1)
-}
int_n :: Integral a => a -> a -> a -> a
int_n n i j = abs ((j - i) `mod` n)

{- | 'L.MonadLogic' all-interval series.

> map (length . L.observeAll . all_interval_m) [4,6,8,10] == [2,4,24,288]
> [0,1,3,2,9,5,10,4,7,11,8,6] `elem` L.observeAll (all_interval_m 12)
> length (L.observeAll (all_interval_m 12)) == 3856
-}
all_interval_m :: (MonadPlus m, L.MonadLogic m) => Int -> m [Int]
all_interval_m n =
  let recur k p q =
        -- k = length p, p = pitch-class sequence, q = interval set
        if k == n
          then return (reverse p)
          else do
            i <- fromList [1 .. n - 1]
            guard (i `notElem` p)
            let j = List.head_err p
                m = int_n n i j
            guard (m `notElem` q)
            recur (k + 1) (i : p) (m : q)
  in recur 1 [0] []

{- | 'L.observeAll' of 'all_interval_m'.

> let r = [[0,1,5,2,4,3],[0,2,1,4,5,3],[0,4,5,2,1,3],[0,5,1,4,2,3]]
> all_interval 6 == r

> d_dx_n n l = zipWith (int_n n) l (tail l)
> map (d_dx_n 6) r == [[1,4,3,2,5],[2,5,3,1,4],[4,1,3,5,2],[5,2,3,4,1]]
-}
all_interval :: Int -> [[Int]]
all_interval = L.observeAll . all_interval_m
