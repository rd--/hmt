-- | Michael Buchler. \"Relative Saturation of Subsets and Interval
-- Cycles as a Means for Determining Set-Class Similarity\". PhD
-- thesis, University of Rochester, 1998
module Music.Theory.Metric.Buchler_1998 where

import Data.List
import Data.Ratio
import qualified Music.Theory.PitchClass as T
import qualified Music.Theory.Table as T

-- | Filter list to for elements with cardinality /n/.
of_c :: Integral n => n -> [a] -> Bool
of_c n = (== n) . genericLength

-- | Set classes of cardinality /n/.
--
-- > sc_table_n 2 == [[0,1],[0,2],[0,3],[0,4],[0,5],[0,6]]
sc_table_n :: (Integral n,Integral a) => n -> [[a]]
sc_table_n n = filter (of_c n) (map snd T.sc_table)

-- | Minima and maxima of ICV of SCs of cardinality /n/.
--
-- > icv_minmax 5 == ([0,0,0,1,0,0],[4,4,4,4,4,2])
icv_minmax :: (Integral n, Integral b) => n -> ([b], [b])
icv_minmax n =
    let t = sc_table_n n
        i = transpose (map T.icv t)
    in (map minimum i,map maximum i)

data R = MIN | MAX deriving (Eq,Show)
type D n = (R,n)

-- | Pretty printer for 'R'.
--
-- > map r_pp [MIN,MAX] == ["+","-"]
r_pp :: R -> String
r_pp r =
    case r of
      MIN -> "+"
      MAX -> "-"

-- | 'SATV' element measure with given funtion.
satv_f :: (Integral n) => ((n,n,n) -> D n) -> [n] -> [D n]
satv_f f p =
    let n = length p
        i = T.icv p
        (l,r) = icv_minmax n
    in map f (zip3 l i r)

-- | Bracket sequence with left and right values.
--
-- > bracket ('<','>') "1,2,3" == "<1,2,3>"
bracket :: (a,a) -> [a] -> [a]
bracket (l,r) x = l : x ++ [r]

-- | Pretty printer for SATV element.
--
-- > satv_e_pp (satv_a [0,1,2,6,7,8]) == "<-1,+2,+0,+0,-1,-0>"
satv_e_pp :: Show i => [D i] -> String
satv_e_pp =
    let f (i,j) = r_pp i ++ show j
    in bracket ('<','>') . intercalate "," . map f

type SATV i = ([D i],[D i])

-- | Pretty printer for 'SATV'.
satv_pp :: Show i => SATV i -> String
satv_pp (i,j) = bracket ('(',')') (satv_e_pp i ++ "," ++ satv_e_pp j)

-- | @SATVa@ measure.
--
-- > satv_e_pp (satv_a [0,1,2,6,7,8]) == "<-1,+2,+0,+0,-1,-0>"
-- > satv_e_pp (satv_a [0,1,2,3,4]) == "<-0,-1,-2,+0,+0,+0>"
satv_a :: Integral i => [i] -> [D i]
satv_a =
    let f (l,i,r) = let l' = abs (i - l)
                        r' = abs (i - r)
                    in case compare l' r' of
                         LT -> (MIN,l')
                         _ -> (MAX,r')
    in satv_f f

-- | @SATVb@ measure.
--
-- > satv_e_pp (satv_b [0,1,2,6,7,8]) == "<+4,-4,-5,-4,+4,+3>"
-- > satv_e_pp (satv_b [0,1,2,3,4]) == "<+4,+3,+2,-3,-4,-2>"
satv_b :: Integral i => [i] -> [D i]
satv_b =
    let f (l,i,r) = let l' = abs (i - l)
                        r' = abs (i - r)
                    in case compare l' r' of
                         LT -> (MAX,r')
                         _ -> (MIN,l')
    in satv_f f

-- | 'SATV' measure.
--
-- > satv_pp (satv [0,3,6,9]) == "(<+0,+0,-0,+0,+0,-0>,<-3,-3,+4,-3,-3,+2>)"
-- > satv_pp (satv [0,1,3,4,8]) == "(<-2,+1,-2,-1,-2,+0>,<+2,-3,+2,+2,+2,-2>)"
-- > satv_pp (satv [0,1,2,6,7,8]) == "(<-1,+2,+0,+0,-1,-0>,<+4,-4,-5,-4,+4,+3>)"
-- > satv_pp (satv [0,4]) == "(<+0,+0,+0,-0,+0,+0>,<-1,-1,-1,+1,-1,-1>)"
-- > satv_pp (satv [0,1,3,4,6,9]) == "(<+2,+2,-0,+0,+2,-1>,<-3,-4,+5,-4,-3,+2>)"
-- > satv_pp (satv [0,1,3,6,7,9]) == "(<+2,+2,-1,+0,+2,-0>,<-3,-4,+4,-4,-3,+3>)"
-- > satv_pp (satv [0,1,2,3,6]) == "(<-1,-2,-2,+0,+1,-1>,<+3,+2,+2,-3,-3,+1>)"
-- > satv_pp (satv [0,1,2,3,4,6]) == "(<-1,-2,-2,+0,+1,+1>,<+4,+4,+3,-4,-4,-2>)"
-- > satv_pp (satv [0,1,3,6,8]) == "(<+1,-2,-2,+0,-1,-1>,<-3,+2,+2,-3,+3,+1>)"
-- > satv_pp (satv [0,2,3,5,7,9]) == "(<+1,-2,-2,+0,-1,+1>,<-4,+4,+3,-4,+4,-2>)"
satv :: Integral i => [i] -> SATV i
satv p = (satv_a p,satv_b p)

-- | 'SATV' reorgaminsed by 'R'.
--
-- > satv_minmax (satv [0,1,2,6,7,8]) == ([4,2,0,0,4,3],[1,4,5,4,1,0])
satv_minmax :: SATV i -> ([i],[i])
satv_minmax (p,q) =
    let f (i,j) (_,k) = if i == MIN then (j,k) else (k,j)
    in unzip (zipWith f p q)

-- | Absolute difference.
abs_dif :: Num a => a -> a -> a
abs_dif i j = abs (i - j)

-- | Sum of numerical components of @a@ and @b@ parts of 'SATV'.
--
-- > satv_n_sum (satv [0,1,2,6,7,8]) == [5,6,5,4,5,3]
-- > satv_n_sum (satv [0,3,6,9]) = [3,3,4,3,3,2]
satv_n_sum :: Num c => SATV c -> [c]
satv_n_sum (i,j) = zipWith (+) (map snd i) (map snd j)

-- > two_part_difference_vector (satv_a [0,1,2,6,7,8]) (satv [0,3,6,9]) == [2,2,4,0,2,0]
two_part_difference_vector :: (Integral i) => [D i] -> SATV i -> [i]
two_part_difference_vector i j =
    let (p,q) = satv_minmax j
        f (r,_) k = if r == MIN then p!!k else q!!k
        z = zipWith f i [0..]
    in zipWith abs_dif (map snd i) z

-- > two_part_difference_vector_set (satv [0,4]) (satv [0,1,3,4,6,9]) == ([2,2,5,4,2,2],[2,2,1,1,2,0])
two_part_difference_vector_set :: (Integral i) => SATV i -> SATV i -> ([i],[i])
two_part_difference_vector_set i j =
        (two_part_difference_vector (fst i) j
        ,two_part_difference_vector (fst j) i)

-- | @SATSIM@ metric.
--
-- > satsim [0,1,2,6,7,8] [0,3,6,9] == 25/46
-- > satsim [0,4] [0,1,3,4,6,9] == 25/34
-- > satsim [0,4] [0,1,3,6,7,9] == 25/34
-- > satsim [0,1,2,3,6] [0,1,2,3,4,6] == 1/49
-- > satsim [0,1,3,6,8] [0,2,3,5,7,9] == 1/49
satsim :: Integral a => [a] -> [a] -> Ratio a
satsim p q =
    let i = satv p
        j = satv q
        (d1,d2) = two_part_difference_vector_set i j
        (n1,n2) = (satv_n_sum i,satv_n_sum j)
    in (sum d1 + sum d2) % (sum n1 + sum n2)
