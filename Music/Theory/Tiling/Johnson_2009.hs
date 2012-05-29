-- | Tom Johnson. \"Tiling in my Music\". /The Experimental Music
-- Yearbook/, 1, 2009.
module Music.Theory.Tiling.Johnson_2009 where

import Data.List
import Data.List.Split {- split -}

-- | Canon of /(period,sequence,multipliers,displacements)/.
type R = (Int,[Int],[Int],[Int])

-- | Voice.
type V = [Int]

-- | Cycle at /period/.
--
-- > take 9 (p_cycle 18 [0,2,5]) == [0,2,5,18,20,23,36,38,41]
p_cycle :: Int -> [Int] -> [Int]
p_cycle n s = s ++ p_cycle n (map (+ n) s)

-- | Set of 'V' from 'R'.
--
-- > v_print 24 (r_voices (12,[0..5],[1,1],[0,6]))
-- > v_print 24 (r_voices (12,[0,2,3,4,5,7],[1,1],[0,6]))
-- > v_print 24 (r_voices (12,[0,1,3,4,5,8],[1,1],[0,6]))
-- > v_print 24 (r_voices (12,[0,3,4,5,7,8],[1,1],[0,6]))
--
-- > let r = (36,[0,1,3],[1,2,3,4,5,6,7,8],[0,7,25,51,52,78,105,130])
-- > in v_print_m_from 18 6 6 (r_voices r)
r_voices :: R -> [V]
r_voices (p,s,m,o) =
    let f (i,j) = p_cycle p (map ((+ j) . (* i)) s)
    in map f (zip m o)

-- | 'concatMap' of 'r_voices'.
--
-- > v_print 36 (rr_voices [(18,[0,2,5],[1,1,1],[0,1,8])
-- >                       ,(18,[0,3,5],[1,1,1],[4,11,12])])
rr_voices :: [R] -> [V]
rr_voices = concatMap r_voices

-- | Variant of 'elem' for ordered sequences, which can therefore
-- return 'False' when searching infinite sequences.
--
-- > 5 `elemOrd` [0,2..] == False
-- > 10 `elemOrd` [0,2..] == True
elemOrd :: Ord a => a -> [a] -> Bool
elemOrd i p =
    case p of
      [] -> False
      j:p' -> case compare j i of
                LT -> elemOrd i p'
                EQ -> True
                GT -> False

-- | A @.*@ diagram of /n/ places of 'V'.
--
-- > v_dot_star 18 [0,2..] == "*.*.*.*.*.*.*.*.*."
v_dot_star :: Int -> V -> String
v_dot_star n v =
    let f p i = if i `elemOrd` p then '*' else '.'
    in map (f v) [0..n-1]

-- | Insert @|@ every /n/ places.
--
-- > with_bars 6 (v_dot_star 18 [0,2..]) == "*.*.*.|*.*.*.|*.*.*."
with_bars :: Int -> String -> String
with_bars m = concat . intersperse "|" . splitEvery m

-- | Variant with measure length /m/ and number of measures /n/.
--
-- > v_dot_star_m 6 3 [0,2..] == "*.*.*.|*.*.*.|*.*.*."
v_dot_star_m :: Int -> Int -> V -> String
v_dot_star_m m n = with_bars m . v_dot_star (n * m)

-- | Print @.*@ diagram.
v_print :: Int -> [V] -> IO ()
v_print n = putStrLn . unlines . ("" :) . map (v_dot_star n)

-- | Variant to print @|@ at measures.
v_print_m :: Int -> Int -> [V] -> IO ()
v_print_m m n = putStrLn . unlines . ("" :) . map (v_dot_star_m m n)

-- | Variant that discards first /k/ measures.
v_print_m_from :: Int -> Int -> Int -> [V] -> IO ()
v_print_m_from k m n =
    let k' = k * m
        f = with_bars m . drop k' . v_dot_star (n * m + k')
    in putStrLn . unlines . ("" :) . map f
