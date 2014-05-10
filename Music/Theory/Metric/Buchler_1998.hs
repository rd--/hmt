-- | Michael Buchler. \"Relative Saturation of Subsets and Interval
-- Cycles as a Means for Determining Set-Class Similarity\". PhD
-- thesis, University of Rochester, 1998
module Music.Theory.Metric.Buchler_1998 where

import Data.List {- base -}
import Data.Ratio {- base -}

import qualified Music.Theory.List as T
import qualified Music.Theory.Z12.Forte_1973 as T
import qualified Music.Theory.Set.List as T
import Music.Theory.Z12 (Z12)

-- | Predicate for list with cardinality /n/.
of_c :: Integral n => n -> [a] -> Bool
of_c n = (== n) . genericLength

-- | Set classes of cardinality /n/.
--
-- > sc_table_n 2 == [[0,1],[0,2],[0,3],[0,4],[0,5],[0,6]]
sc_table_n :: (Integral n) => n -> [[Z12]]
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
satv_f :: (Integral n) => ((n,n,n) -> D n) -> [Z12] -> [D n]
satv_f f p =
    let n = length p
        i = T.icv p
        (l,r) = icv_minmax n
    in map f (zip3 l i r)

-- | Pretty printer for SATV element.
--
-- > satv_e_pp (satv_a [0,1,2,6,7,8]) == "<-1,+2,+0,+0,-1,-0>"
satv_e_pp :: Show i => [D i] -> String
satv_e_pp =
    let f (i,j) = r_pp i ++ show j
    in T.bracket ('<','>') . intercalate "," . map f

type SATV i = ([D i],[D i])

-- | Pretty printer for 'SATV'.
satv_pp :: Show i => SATV i -> String
satv_pp (i,j) = T.bracket ('(',')') (satv_e_pp i ++ "," ++ satv_e_pp j)

-- | @SATVa@ measure.
--
-- > satv_e_pp (satv_a [0,1,2,6,7,8]) == "<-1,+2,+0,+0,-1,-0>"
-- > satv_e_pp (satv_a [0,1,2,3,4]) == "<-0,-1,-2,+0,+0,+0>"
satv_a :: Integral i => [Z12] -> [D i]
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
satv_b :: Integral i => [Z12] -> [D i]
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
satv :: Integral i => [Z12] -> SATV i
satv p = (satv_a p,satv_b p)

-- | 'SATV' reorganised by 'R'.
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
-- > satsim [0,1,2,3,4] [0,1,4,5,7] == 8/21
-- > satsim [0,1,2,3,4] [0,2,4,6,8] == 4/7
-- > satsim [0,1,4,5,7] [0,2,4,6,8] == 4/7
satsim :: Integral a => [Z12] -> [Z12] -> Ratio a
satsim p q =
    let i = satv p
        j = satv q
        (d1,d2) = two_part_difference_vector_set i j
        d = sum d1 + sum d2
        (n1,n2) = (satv_n_sum i,satv_n_sum j)
        n = sum n1 + sum n2
    in if n == 0 then error (show ("satsim",p,q)) else d % n

-- | Table of 'satsim' measures for all @SC@ pairs.
--
-- > length satsim_table == 24310
satsim_table :: Integral i => [(([Z12],[Z12]),Ratio i)]
satsim_table =
    let f (i,j) = ((i,j),satsim i j)
        t = filter ((`notElem` [0,1,12]) . length) (map snd T.sc_table)
    in map f (T.pairs t)

-- | Histogram of values at 'satsim_table'.
--
-- > satsim_table_histogram == T.histogram (map snd satsim_table)
satsim_table_histogram :: Integral i => [(Ratio i,i)]
satsim_table_histogram = [(0,132),(1/49,4),(1/30,4),(2/49,16),(2/39,16),(18,8),(2/33,12),(3/49,30),(15,12),(14,144),(13,56),(4/49,72),(2/23,14),(2/21,304),(10,6),(5/49,132),(4/39,160),(1/9,264),(4/33,16),(6/49,152),(1/8,12),(5/39,108),(3/23,4),(25,44),(1/7,487),(7/46,6),(23,132),(8/49,304),(1/6,116),(4/23,86),(7/40,6),(7/39,444),(21,48),(9/49,208),(4/21,1116),(9/46,84),(1/5,68),(10/49,298),(8/39,472),(5/24,4),(7/33,88),(34,394),(5/23,176),(2/9,516),(11/49,378),(9/40,8),(33,176),(7/30,116),(11/46,172),(8/33,64),(12/49,314),(1/4,10),(10/39,336),(7/27,4),(6/23,276),(9/34,2),(13/49,374),(45,124),(31,192),(11/40,4),(58,56),(11/39,376),(13/46,298),(2/7,1297),(7/24,48),(8/27,8),(30,226),(10/33,148),(7/23,204),(15/49,228),(43,384),(11/34,6),(13/40,50),(15/46,272),(16/49,196),(1/3,1528),(17/49,132),(8/23,230),(7/20,128),(67,6),(54,82),(14/39,144),(41,160),(11/30,168),(18/49,74),(17/46,228),(10/27,32),(3/8,238),(8/21,412),(53,160),(19/49,84),(78,76),(9/23,94),(13/33,284),(2/5,310),(11/27,44),(20/49,76),(16/39,376),(77,14),(19/46,150),(52,128),(14/33,156),(17/40,154),(3/7,81),(13/30,108),(10/23,114),(17/39,236),(15/34,4),(4/9,460),(22/49,10),(9/20,96),(51,172),(21/46,124),(11/24,144),(63,112),(75,84),(23/49,6),(87,28),(19/40,96),(10/21,84),(11/23,28),(13/27,188),(16/33,52),(19/39,160),(24/49,8),(1/2,545),(25/49,2),(20/39,144),(17/33,100),(14/27,296),(12/23,64),(21/40,42),(97,48),(85,56),(15/28,1),(73,64),(13/24,32),(25/46,66),(61,36),(11/20,18),(27/49,24),(5/9,192),(19/34,132),(22/39,24),(13/23,18),(17/30,40),(4/7,176),(23/40,32),(19/33,16),(72,28),(27/46,56),(107,84),(23/39,20),(29/49,26),(16/27,72),(3/5,14),(20/33,4),(14/23,10),(30/49,24),(21/34,120),(5/8,28),(17/27,36),(31/49,22),(71,16),(94,22),(117,72),(13/20,4),(32/49,14),(2/3,14),(27/40,6),(23/34,14),(19/28,1),(70,4),(19/27,4),(127,24),(5/7,10),(25/34,4),(3/4,7),(7/9,12),(114,4),(17/21,4),(23/28,7),(5/6,20),(6/7,11),(8/9,12),(25/28,16),(19/21,38),(112,4),(134,7),(178,18),(20/21,12),(1,32)]

-- Local Variables:
-- truncate-lines:t
-- End:
