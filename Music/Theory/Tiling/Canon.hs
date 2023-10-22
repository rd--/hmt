module Music.Theory.Tiling.Canon where

import Control.Monad {- base -}
import Data.List {- base -}
import Data.List.Split {- split -}
import Text.Printf {- base -}

import qualified Control.Monad.Logic as L {- logict -}

import qualified Music.Theory.List as List {- hmt -}

-- | Sequence.
type S = [Int]

-- | Canon of /(period,sequence,multipliers,displacements)/.
type R = (Int,S,[Int],[Int])

-- | Voice.
type V = [Int]

-- | Tiling (sequence)
type T = [[Int]]

{- | Cycle at /period/.

>>> take 9 (p_cycle 18 [0,2,5])
[0,2,5,18,20,23,36,38,41]
-}
p_cycle :: Int -> [Int] -> [Int]
p_cycle n s = s ++ p_cycle n (map (+ n) s)

-- | Element of /(sequence,multiplier,displacement)/.
type E = (S,Int,Int)

{- | Resolve sequence from 'E'.

>>> e_to_seq ([0,2,5],2,1)
[1,5,11]

>>> e_to_seq ([0,1],3,4)
[4,7]

>>> e_to_seq ([0],1,2)
[2]
-}
e_to_seq :: E -> [Int]
e_to_seq (s,m,o) = map ((+ o) . (* m)) s

{- | Infer 'E' from sequence.

>>> e_from_seq [1,5,11]
([0,2,5],2,1)

>>> e_from_seq [4,7]
([0,1],3,4)

>>> e_from_seq [2]
([0],1,2)
-}
e_from_seq :: [Int] -> E
e_from_seq p =
    let i = List.head_err p
        q = map (+ negate i) p
        r = List.tail_err q
        n = if null r then 1 else foldl1 gcd r
    in (map (`div` n) q,n,i)

-- | Set of 'V' from 'R'.
r_voices :: R -> [V]
r_voices (p,s,m,o) =
    let f i j = p_cycle p (e_to_seq (s,i,j))
    in zipWith f m o

-- | 'concatMap' of 'r_voices'.
rr_voices :: [R] -> [V]
rr_voices = concatMap r_voices

{- | Retrograde of 'T', the result 'T' is sorted.

>>> t_retrograde [[0,7,14],[1,6,11],[2,3,4],[5,9,13],[8,10,12]]
[[0,7,14],[1,5,9],[2,4,6],[3,8,13],[10,11,12]]
-}
t_retrograde :: T -> T
t_retrograde t =
    let n = maximum (concat t)
    in sort (map (reverse . map (n -)) t)

{- | The normal form of 'T' is the 'min' of /t/ and it's 't_retrograde'.

>>> t_normal [[0,7,14],[1,6,11],[2,3,4],[5,9,13],[8,10,12]]
[[0,7,14],[1,5,9],[2,4,6],[3,8,13],[10,11,12]]
-}
t_normal :: T -> T
t_normal t = min t (t_retrograde t)

{- | Derive set of 'R' from 'T'.

>>> let r = [(21,[0,1,2],[10,8,2,4,7,5,1],[0,1,2,3,5,8,14])]
>>> let t = [[0,10,20],[1,9,17],[2,4,6],[3,7,11],[5,12,19],[8,13,18],[14,15,16]]
>>> r_from_t t == r
True
-}
r_from_t :: T -> [R]
r_from_t t =
    let e = map e_from_seq t
        n = maximum (concat t) + 1
        t3_1 (i,_,_) = i
        f z = let (s,m,o) = unzip3 z in (n,List.head_err s,m,o)
    in map f (List.group_on t3_1 e)

-- * Construction

{- | 'msum' '.' 'map' 'return'.

>>> L.observeAll (fromList [1..7])
[1,2,3,4,5,6,7]
-}
fromList :: MonadPlus m => [a] -> m a
fromList = msum . map return

-- | Search for /perfect/ tilings of the sequence 'S' using
-- multipliers from /m/ to degree /n/ with /k/ parts.
perfect_tilings_m :: MonadPlus m => [S] -> [Int] -> Int -> Int -> m T
perfect_tilings_m s m n k =
    let rec p q =
            if length q == k
            then return (sort q)
            else do m' <- fromList m
                    guard (m' `notElem` p)
                    s' <- fromList s
                    let i = n - (maximum s' * m') - 1
                    o <- fromList [0..i]
                    let s'' = e_to_seq (s',m',o)
                        q' = concat q
                    guard (all (`notElem` q') s'')
                    rec (m':p) (s'':q)
    in rec [] []

{- | 't_normal' of 'L.observeAll' of 'perfect_tilings_m'.

>>> perfect_tilings [[0,1]] [1..3] 6 3
[]

>>> perfect_tilings [[0,1,2]] [1,2,4,5,7] 15 5
[[[0,7,14],[1,5,9],[2,4,6],[3,8,13],[10,11,12]]]

>>> length (perfect_tilings [[0,1,2]] [1..12] 15 5)
1

>>> perfect_tilings [[0,1]] [1..4] 8 4
[[[0,1],[2,5],[3,7],[4,6]],[[0,1],[2,6],[3,5],[4,7]],[[0,2],[1,4],[3,7],[5,6]]]

>>> perfect_tilings [[0,1]] [1..5] 10 5
[[[0,1],[2,5],[3,7],[4,9],[6,8]],[[0,1],[2,7],[3,5],[4,8],[6,9]],[[0,2],[1,4],[3,8],[5,9],[6,7]],[[0,2],[1,5],[3,6],[4,9],[7,8]],[[0,3],[1,6],[2,4],[5,9],[7,8]]]

Johnson 2004, p.2

> perfect_tilings [[0,1,2]] [1,2,3,5,6,7,9] 21 7 -- slow
[[[0,6,12],[1,8,15],[2,11,20],[3,5,7],[4,9,14],[10,13,16],[17,18,19]]]

> let r = [[0,10,20],[1,9,17],[2,4,6],[3,7,11],[5,12,19],[8,13,18],[14,15,16]]
> perfect_tilings [[0,1,2]] [1,2,4,5,7,8,10] 21 7 == [t_retrograde r] -- slow
True

-}
perfect_tilings :: [S] -> [Int] -> Int -> Int -> [T]
perfect_tilings s m n =
    nub . sort . map t_normal . L.observeAll . perfect_tilings_m s m n

-- * Display

{- | Variant of 'elem' for ordered sequences, which can therefore return 'False' when searching infinite sequences.

>>> 5 `elemOrd` [0,2..]
False

>>> 10 `elemOrd` [0,2..]
True
-}
elemOrd :: Ord a => a -> [a] -> Bool
elemOrd i p =
    case p of
      [] -> False
      j:p' -> case compare j i of
                LT -> elemOrd i p'
                EQ -> True
                GT -> False

{- | A @.*@ diagram of /n/ places of 'V'.

>>> v_dot_star 18 [0,2..]
"*.*.*.*.*.*.*.*.*."
-}
v_dot_star :: Int -> V -> String
v_dot_star n v =
    let f p i = if i `elemOrd` p then '*' else '.'
    in map (f v) [0..n-1]

{- | A white space and index diagram of /n/ places of 'V'.

>>> map (v_space_ix 9) [[0,2..],[1,3..]]
["0   2   4   6   8","  1   3   5   7  "]
-}
v_space_ix :: Int -> V -> String
v_space_ix n v =
    let w = length (show n)
        nil = replicate w ' '
        f p i = if i `elemOrd` p then printf "%*d" w i else nil
    in unwords (map (f v) [0..n-1])

{- | Insert @|@ every /n/ places.

>>> with_bars 6 (v_dot_star 18 [0,2..])
"*.*.*.|*.*.*.|*.*.*."
-}
with_bars :: Int -> String -> String
with_bars m = intercalate "|" . chunksOf m

{- | Variant with measure length /m/ and number of measures /n/.

>>> v_dot_star_m 6 3 [0,2..]
"*.*.*.|*.*.*.|*.*.*."
-}
v_dot_star_m :: Int -> Int -> V -> String
v_dot_star_m m n = with_bars m . v_dot_star (n * m)

-- | Print @.*@ diagram.
v_show :: Int -> [V] -> String
v_show n = unlines . ("" :) . map (v_dot_star n)

v_print :: Int -> [V] -> IO ()
v_print n = putStrLn . v_show n

-- | Variant to print @|@ at measures.
v_show_m :: Int -> Int -> [V] -> String
v_show_m m n = unlines . ("" :) . map (v_dot_star_m m n)

v_print_m :: Int -> Int -> [V] -> IO ()
v_print_m m n = putStrLn . v_show_m m n

-- | Variant that discards first /k/ measures.
v_show_m_from :: Int -> Int -> Int -> [V] -> String
v_show_m_from k m n =
    let k' = k * m
        f = with_bars m . drop k' . v_dot_star (n * m + k')
    in unlines . ("" :) . map f

v_print_m_from :: Int -> Int -> Int -> [V] -> IO ()
v_print_m_from k m n = putStrLn . v_show_m_from k m n
