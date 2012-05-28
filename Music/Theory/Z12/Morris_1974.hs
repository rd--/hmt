-- | Robert Morris and D. Starr. \"The Structure of All-Interval Series\".
-- /Journal of Music Theory/, 18:364-389, 1974.
module Music.Theory.Z12.Morris_1974 where

import Control.Monad.Logic {- logict -}

-- | 'msum' '.' 'map' 'return'.
--
-- > observeAll (fromList [1..7]) == [1..7]
fromList :: MonadPlus m => [a] -> m a
fromList = msum . map return

-- | 'MonadPlus' all-interval series.
--
-- > [0,1,3,2,9,5,10,4,7,11,8,6] `elem` observeAll (all_interval_m 12)
-- > length (observeAll (all_interval_m 12)) == 3856
-- > map (length . observeAll . all_interval_m) [4,6,8,10] == [2,4,24,288]
all_interval_m :: MonadPlus m => Int -> m [Int]
all_interval_m n =
    let rec p q =
            if length p == n
            then return (reverse p)
            else do i <- fromList [1 .. n - 1]
                    guard (i `notElem` p)
                    let j:_ = p
                        m = abs ((i - j) `mod` n)
                    guard (m `notElem` q)
                    rec (i:p) (m:q)
    in rec [0] []

-- | 'observeAll' of 'all_interval_m'.
--
-- > let r = [[0,1,5,2,4,3],[0,2,1,4,5,3],[0,4,5,2,1,3],[0,5,1,4,2,3]]
-- > in all_interval 6 == r
all_interval :: Int -> [[Int]]
all_interval = observeAll . all_interval_m
