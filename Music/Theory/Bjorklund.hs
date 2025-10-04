{- | Godfried T. Toussaint et. al.
\"The distance geometry of music\"
/Journal of Computational Geometry: Theory and Applications/
Volume 42, Issue 5, July, 2009
(<http://dx.doi.org/10.1016/j.comgeo.2008.04.005>)

Erik D. Demaine, et. al..
\"The Distance Geometry of Deep Rhythms and Scales\".
/CCCG/ 2005: 163-166
(<https://erikdemaine.org/papers/DeepRhythms_CCCG2005/paper.pdf>)
-}
module Music.Theory.Bjorklund where

import Data.List {- base -}

import Data.List.Split {- split -}

import qualified Music.Theory.List as List {- hmt-base -}

-- | Bjorklund state
type Bjorklund a = ((Int, Int), ([[a]], [[a]]))

-- | Bjorklund left process
bjorklund_left_step :: Bjorklund a -> Bjorklund a
bjorklund_left_step ((i, j), (xs, ys)) =
  let (xs', xs'') = splitAt j xs
  in ((j, i - j), (zipWith (++) xs' ys, xs''))

-- | Bjorklund right process
bjorklund_right_step :: Bjorklund a -> Bjorklund a
bjorklund_right_step ((i, j), (xs, ys)) =
  let (ys', ys'') = splitAt i ys
  in ((i, j - i), (zipWith (++) xs ys', ys''))

-- | Bjorklund process, left or right or halt.
bjorklund_step :: Bjorklund a -> Maybe (Bjorklund a)
bjorklund_step (n, x) =
  let (i, j) = n
  in if min i j <= 1
      then Nothing
      else
        Just
          ( if i > j
              then bjorklund_left_step (n, x)
              else bjorklund_right_step (n, x)
          )

-- | All intermediate states of Bjorklund process.
bjorklund_sequences :: (t, t) -> (Int, Int) -> [Bjorklund t]
bjorklund_sequences tf =
  let recur r st =
        case bjorklund_step st of
          Nothing -> reverse (st : r)
          Just st' -> recur (st : r) st'
  in recur [] . bjorklund_init tf

-- | Bjorklund initial state.
bjorklund_init :: (t, t) -> (Int, Int) -> Bjorklund t
bjorklund_init (t, f) (i, j') =
  let j = j' - i
      x = replicate i [t]
      y = replicate j [f]
  in ((i, j), (x, y))

{- | Bjorklund state to sequence.

>>> let sq = bjorklund_sequences (True, False) (5, 13)
>>> mapM_ (putStrLn . map xdot_unicode . bjorklund_seq) sq
×××××········
×·×·×·×·×····
×··×··×··×·×·
×··×·×··×·×··
-}
bjorklund_seq :: Bjorklund t -> [t]
bjorklund_seq (_, (x, y)) = concat x ++ concat y

-- | (k,n) -> sequence
type EuclideanAlgorithm = (Int, Int) -> [Bool]

{- | Bjorklund's algorithm to construct a binary sequence of /n/ bits
with /k/ ones such that the /k/ ones are distributed as evenly as
possible among the (/n/ - /k/) zeroes.

>>> bjorklund (5, 9)
[True,False,True,False,True,False,True,False,True]

>>> map xdot_ascii (bjorklund (5, 9))
"x.x.x.x.x"

>>> let es = [(2,[3,5]),(3,[4,5,8]),(4,[7,9,12,15]),(5,[6,7,8,9,11,12,13,16]),(6,[7,13]),(7,[8,9,10,12,15,16,17,18]),(8,[17,19]),(9,[14,16,22,23]),(11,[12,24]),(13,[24]),(15,[34])]
>>> length es
11

>>> let es' = concatMap (\(i,j) -> map ((,) i) j) es
>>> length es'
37

> mapM_ (putStrLn . euler_pp_unicode bjorklund) es'

@
E(2,3) [××·] (12)
E(2,5) [×·×··] (23)
E(3,4) [×××·] (112)
E(3,5) [×·×·×] (221)
E(3,8) [×··×··×·] (332)
E(4,7) [×·×·×·×] (2221)
E(4,9) [×·×·×·×··] (2223)
E(4,12) [×··×··×··×··] (3333)
E(4,15) [×···×···×···×··] (4443)
E(5,6) [×××××·] (11112)
E(5,7) [×·××·××] (21211)
E(5,8) [×·××·××·] (21212)
E(5,9) [×·×·×·×·×] (22221)
E(5,11) [×·×·×·×·×··] (22223)
E(5,12) [×··×·×··×·×·] (32322)
E(5,13) [×··×·×··×·×··] (32323)
E(5,16) [×··×··×··×··×···] (33334)
E(6,7) [××××××·] (111112)
E(6,13) [×·×·×·×·×·×··] (222223)
E(7,8) [×××××××·] (1111112)
E(7,9) [×·×××·×××] (2112111)
E(7,10) [×·××·××·××] (2121211)
E(7,12) [×·××·×·××·×·] (2122122)
E(7,15) [×·×·×·×·×·×·×··] (2222223)
E(7,16) [×··×·×·×··×·×·×·] (3223222)
E(7,17) [×··×·×··×·×··×·×·] (3232322)
E(7,18) [×··×·×··×·×··×·×··] (3232323)
E(8,17) [×·×·×·×·×·×·×·×··] (22222223)
E(8,19) [×··×·×·×··×·×·×··×·] (32232232)
E(9,14) [×·××·××·××·××·] (212121212)
E(9,16) [×·××·×·×·××·×·×·] (212221222)
E(9,22) [×··×·×··×·×··×·×··×·×·] (323232322)
E(9,23) [×··×·×··×·×··×·×··×·×··] (323232323)
E(11,12) [×××××××××××·] (11111111112)
E(11,24) [×··×·×·×·×·×··×·×·×·×·×·] (32222322222)
E(13,24) [×·××·×·×·×·×·××·×·×·×·×·] (2122222122222)
E(15,34) [×··×·×·×·×··×·×·×·×··×·×·×·×··×·×·] (322232223222322)
@
-}
bjorklund :: EuclideanAlgorithm
bjorklund =
  bjorklund_seq
    . List.last_err
    . bjorklund_sequences (True, False)

{- | 'List.rotate_right' of 'bjorklund'.

>>> map xdot_unicode (bjorklund_r 2 (5,16)) == "··×··×··×··×··×·"
True
-}
bjorklund_r :: Int -> EuclideanAlgorithm
bjorklund_r n = List.rotate_right n . bjorklund

-- | Pretty printer, generalise.
euler_pp_f :: EuclideanAlgorithm -> (Bool -> Char) -> (Int, Int) -> String
euler_pp_f b f e =
  let r = b e
  in concat ["E", show e, " [", map f r, "] ", iseq_str r]

{- | Unicode form, ie. @×·@.

>>> euler_pp_unicode bjorklund (7,12) == "E(7,12) [×·××·×·××·×·] (2122122)"
True
-}
euler_pp_unicode :: EuclideanAlgorithm -> (Int, Int) -> String
euler_pp_unicode b = euler_pp_f b xdot_unicode

{- | Ascii form, ie. @x.@.

>>> euler_pp_ascii bjorklund (7,12)
"E(7,12) [x.xx.x.xx.x.] (2122122)"
-}
euler_pp_ascii :: EuclideanAlgorithm -> (Int, Int) -> String
euler_pp_ascii b = euler_pp_f b xdot_ascii

{- | /xdot/ notation for pattern.

>>> map xdot_ascii (bjorklund (5,9))
"x.x.x.x.x"
-}
xdot_ascii :: Bool -> Char
xdot_ascii x = if x then 'x' else '.'

{- | Unicode variant.

>>> map xdot_unicode (bjorklund (5,12)) == "×··×·×··×·×·"
True

>>> map xdot_unicode (bjorklund (5,16)) == "×··×··×··×··×···"
True
-}
xdot_unicode :: Bool -> Char
xdot_unicode x = if x then '×' else '·'

{- | The 'iseq' of a pattern is the distance between 'True' values.

>>> iseq (bjorklund (5,9))
[2,2,2,2,1]
-}
iseq :: [Bool] -> [Int]
iseq = let f = split . keepDelimsL . whenElt in List.tail_err . map length . f (== True)

{- | 'iseq' of pattern as compact string.

>>> iseq_str (bjorklund (5,9))
"(22221)"
-}
iseq_str :: [Bool] -> String
iseq_str = let f xs = "(" ++ concatMap show xs ++ ")" in f . iseq

{- | Bresenham's algorithm.

>>> bresenham (5, 12) == bjorklund(5, 12)
True

Given es' above compare algorithms:

> map (\p -> bresenham p == bjorklund p) es'
> let vw = map xdot_ascii
> mapM_ (\p -> let (a, b) = (bresenham p, bjorklund p) in print (p, vw a, vw b, a == b)) es'

@
((2,3),"x.x","xx.",False)
((2,5),"x..x.","x.x..",False)
((3,4),"x.xx","xxx.",False)
((3,5),"x.x.x","x.x.x",True)
((3,8),"x..x..x.","x..x..x.",True)
((4,7),"x.x.x.x","x.x.x.x",True)
((4,9),"x..x.x.x.","x.x.x.x..",False)
((4,12),"x..x..x..x..","x..x..x..x..",True)
((4,15),"x...x...x...x..","x...x...x...x..",True)
((5,6),"x.xxxx","xxxxx.",False)
((5,7),"x.xx.xx","x.xx.xx",True)
((5,8),"x.x.xx.x","x.xx.xx.",False)
((5,9),"x.x.x.x.x","x.x.x.x.x",True)
((5,11),"x..x.x.x.x.","x.x.x.x.x..",False)
((5,12),"x..x.x..x.x.","x..x.x..x.x.",True)
((5,13),"x..x..x.x..x.","x..x.x..x.x..",False)
((5,16),"x...x..x..x..x..","x..x..x..x..x...",False)
((6,7),"x.xxxxx","xxxxxx.",False)
((6,13),"x..x.x.x.x.x.","x.x.x.x.x.x..",False)
((7,8),"x.xxxxxx","xxxxxxx.",False)
((7,9),"x.xxx.xxx","x.xxx.xxx",True)
((7,10),"x.xx.xx.xx","x.xx.xx.xx",True)
((7,12),"x.x.x.xx.x.x","x.xx.x.xx.x.",False)
((7,15),"x..x.x.x.x.x.x.","x.x.x.x.x.x.x..",False)
((7,16),"x..x.x.x..x.x.x.","x..x.x.x..x.x.x.",True)
((7,17),"x..x.x..x.x..x.x.","x..x.x..x.x..x.x.",True)
((7,18),"x..x..x.x..x.x..x.","x..x.x..x.x..x.x..",False)
((8,17),"x..x.x.x.x.x.x.x.","x.x.x.x.x.x.x.x..",False)
((8,19),"x..x.x..x.x.x..x.x.","x..x.x.x..x.x.x..x.",False)
((9,14),"x.x.xx.xx.xx.x","x.xx.xx.xx.xx.",False)
((9,16),"x.x.x.x.xx.x.x.x","x.xx.x.x.xx.x.x.",False)
((9,22),"x..x.x..x.x..x.x..x.x.","x..x.x..x.x..x.x..x.x.",True)
((9,23),"x..x..x.x..x.x..x.x..x.","x..x.x..x.x..x.x..x.x..",False)
((11,12),"x.xxxxxxxxxx","xxxxxxxxxxx.",False)
((11,24),"x..x.x.x.x.x..x.x.x.x.x.","x..x.x.x.x.x..x.x.x.x.x.",True)
((13,24),"x.x.x.x.x.x.xx.x.x.x.x.x","x.xx.x.x.x.x.xx.x.x.x.x.",False)
((15,34),"x..x.x.x..x.x.x.x..x.x.x.x..x.x.x.","x..x.x.x.x..x.x.x.x..x.x.x.x..x.x.",False)
@
-}
bresenham :: (Int, Int) -> [Bool]
bresenham (s, n) =
  let f d = if d >= 0 then True : f (d + s - n) else False : f (d + s)
  in take n (f 0)

{- | Initial matrix for matrix variant of Bjorklund algorithm.

>>> bjorklund_matrix_init (1, 0) (5, 13)
[[1,1,1,1,1,0,0,0],[0,0,0,0,0]]

>>> bjorklund_matrix_init (1, 0) (5, 9)
[[1,1,1,1,1],[0,0,0,0]]

>>> bjorklund_matrix_init (1, 0) (2, 3)
[[1,1],[0]]

>>> bjorklund_matrix_init (1, 0) (2, 4)
[[1,1],[0,0]]

>>> bjorklund_matrix_init (1, 0) (4, 15)
[[1,1,1,1,0,0,0,0,0,0,0],[0,0,0,0]]
-}
bjorklund_matrix_init :: (t, t) -> (Int, Int) -> [[t]]
bjorklund_matrix_init (t, f) (k, n) =
  let i = n - k
      m = max (i - k) 0
  in [ replicate k t ++ replicate (i - k) f
     , replicate (i - m) f
     ]

{- | Step intermediate matrix, or halt.

>>> bjorklund_matrix_step [[1,1,1,1,1,0,0,0],[0,0,0,0,0]]
Just [[1,1,1,1,1],[0,0,0,0,0],[0,0,0]]

>>> bjorklund_matrix_step [[1,1,1,1,1],[0,0,0,0,0],[0,0,0]]
Just [[1,1,1],[0,0,0],[0,0,0],[1,1],[0,0]]

>>> bjorklund_matrix_step [[1,1,1],[0,0,0],[0,0,0],[1,1],[0,0]]
Nothing

>>> bjorklund_matrix_step [[1,1],[0]]
Nothing

>>> bjorklund_matrix_step [[1,1],[0,0]]
Nothing

>>> bjorklund_matrix_step [[1,1,1,1,0,0,0,0,0,0,0],[0,0,0,0]]
Just [[1,1,1,1],[0,0,0,0],[0,0,0,0,0,0,0]]
-}
bjorklund_matrix_step :: [[t]] -> Maybe [[t]]
bjorklund_matrix_step m =
  let l = sort (nub (map length m))
      p = minimum l
      q = maximum l
  in if p == 1 || (q - p <= 1)
      then Nothing
      else
        let a = map (take p) m
            b = filter (not . null) (map (drop p) m)
        in Just (a ++ b)

{- | The sequence of matrices.

>>> let m = bjorklund_matrices (1, 0) (5, 13)
>>> m !! 0
[[1,1,1,1,1,0,0,0],[0,0,0,0,0]]

>>> m !! 2
[[1,1,1],[0,0,0],[0,0,0],[1,1],[0,0]]

>>> let pp = concat . map show
>>> mapM_ (putStr . unlines . map pp) m
11111000
00000
11111
00000
000
111
000
000
11
00
-}
bjorklund_matrices :: (t, t) -> (Int, Int) -> [[[t]]]
bjorklund_matrices tf kn =
  let m0 = bjorklund_matrix_init tf kn
      recur r m = case bjorklund_matrix_step m of
        Just m' -> recur (m' : r) m'
        Nothing -> reverse r
  in recur [m0] m0

{- | Each matrix of bjorklund_matrices, as a sequence.

>>> let seq = bjorklund_matrices_sequences (True, False) (5, 13)
>>> mapM_ (putStrLn . map xdot_unicode) seq
×·×·×·×·×····
×··×··×··×·×·
×··×·×··×·×··
-}
bjorklund_matrices_sequences :: (t, t) -> (Int, Int) -> [[t]]
bjorklund_matrices_sequences tf =
  let f = concat . transpose
  in map f . bjorklund_matrices tf

{- | Bjorklund by matrix method.

>>> euler_pp_ascii bjorklund (5,9)
"E(5,9) [x.x.x.x.x] (22221)"

>>> euler_pp_ascii bjorklundM (5,9)
"E(5,9) [x.x.x.x.x] (22221)"

Given es' above compare algorithms:

> mapM_ (\x -> putStrLn (euler_pp_unicode bjorklundM x)) es'

@
E(2,3) [×·×] (21)
E(2,5) [×·×··] (23)
E(3,4) [×·××] (211)
E(3,5) [×·×·×] (221)
E(3,8) [×··×··×·] (332)
E(4,7) [×·×·×·×] (2221)
E(4,9) [×·×·×·×··] (2223)
E(4,12) [×··×··×··×··] (3333)
E(4,15) [×···×···×···×··] (4443)
E(5,6) [×·××××] (21111)
E(5,7) [×·××·××] (21211)
E(5,8) [×·××·××·] (21212)
E(5,9) [×·×·×·×·×] (22221)
E(5,11) [×·×·×·×·×··] (22223)
E(5,12) [×··×·×··×·×·] (32322)
E(5,13) [×··×·×··×·×··] (32323)
E(5,16) [×··×··×··×··×···] (33334)
E(6,7) [×·×××××] (211111)
E(6,13) [×·×·×·×·×·×··] (222223)
E(7,8) [×·××××××] (2111111)
E(7,9) [×·×××·×××] (2112111)
E(7,10) [×·××·××·××] (2121211)
E(7,12) [×·××·×·××·×·] (2122122)
E(7,15) [×·×·×·×·×·×·×··] (2222223)
E(7,16) [×··×·×·×··×·×·×·] (3223222)
E(7,17) [×··×·×··×·×··×·×·] (3232322)
E(7,18) [×··×·×··×·×··×·×··] (3232323)
E(8,17) [×·×·×·×·×·×·×·×··] (22222223)
E(8,19) [×··×·×·×··×·×·×··×·] (32232232)
E(9,14) [×·××·××·××·××·] (212121212)
E(9,16) [×·××·×·×·××·×·×·] (212221222)
E(9,22) [×··×·×··×·×··×·×··×·×·] (323232322)
E(9,23) [×··×·×··×·×··×·×··×·×··] (323232323)
E(11,12) [×·××××××××××] (21111111111)
E(11,24) [×··×·×·×·×·×··×·×·×·×·×·] (32222322222)
E(13,24) [×·××·×·×·×·×·××·×·×·×·×·] (2122222122222)
E(15,34) [×··×·×·×·×··×·×·×·×··×·×·×·×··×·×·] (322232223222322)
@
-}
bjorklundM :: EuclideanAlgorithm
bjorklundM = List.last_err . bjorklund_matrices_sequences (True, False)

{- | The oriented distance from onset i to onset j in R/n.
The length of the counterclockwise arc starting at i and ending at j
on the circle of circumference n.
-}
orientedDistance :: (Ord a, Num a) => a -> a -> a -> a
orientedDistance i j n =
  if i < j
  then j - i
  else n + j - i

{- | The distance between two onsets i and j in R.
The minimum of the oriented distance from i to j
and the oriented distance from j to i.
I.e. the length of the shortest arc connecting points i and j
on the circle of circumference n.
-}
distance :: (Ord a, Num a) => a -> a -> a -> a
distance i j n = min (abs (i - j)) (n - abs (i - j))

{- | The distance multiset of a rhythm R
is the multiset of all nonzero pairwise distances.

>>> distanceMultiset [0,4,5,9,10,14,15] 16
[4,5,7,6,2,1,1,5,6,6,5,4,5,7,6,1,5,6,4,5,1]
-}
distanceMultiset :: (Ord a, Num a) => [a] -> a -> [a]
distanceMultiset l n =
  [distance i j n |
    i <- l,
    j <- l,
    i < j]

{- | A rhythm is Erdos-deep if it has (exactly) one distance of multiplicity i,
for i in 1 to k−1.

>>> isErdosDeep [0,4,5,9,10,14,15] 16
True

>>> isErdosDeep [0,2,4,5,7,9,11] 12
True

>>> isErdosDeep [0,4,7] 10
True
-}
isErdosDeep :: (Num a, Ord a) => [a] -> a -> Bool
isErdosDeep l n =
  let k = length l
      d = distanceMultiset l n
      h = List.histogram d
      q = sort (map snd h)
  in q == [1 .. k - 1]

{- | A rhythm is Winograd-deep if every two possible distances in (1 .. n/2)
have different multiplicity.

>>> isWinogradDeep [0,4,5,9,10,14,15] 16
False

>>> isWinogradDeep [0,2,4,5,7,9,11] 12
True

>>> isWinogradDeep [0,4,7] 10
False

>>> isWinogradDeep [0,2,3,5,7,9,11,13,15] 17
True
-}
isWinogradDeep :: Integral t => [t] -> t -> Bool
isWinogradDeep l n =
  let d = distanceMultiset l n
      h = List.generic_histogram d
      q = sort (map snd h)
  in q == [1 .. n `div` 2]
