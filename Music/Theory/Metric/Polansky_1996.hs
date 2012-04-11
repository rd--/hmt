-- | Larry Polansky. \"Morphological Metrics\". Journal of New Music
-- Research, 25(4):289-368, 1996.
module Music.Theory.Metric.Polansky_1996 where

import Data.List
import Data.Maybe
import Data.Ratio
import qualified Music.Theory.Contour.Polansky_1992 as T
import qualified Music.Theory.List as T

class Interval a where interval :: Fractional n => a -> a -> n
instance Interval Int where interval i j = fromIntegral (i - j)
instance Interval Integer where interval i j = fromIntegral (i - j)
instance Interval Float where interval i j = realToFrac (i - j)
instance Interval Double where interval i j = realToFrac (i - j)

abs_dif :: (Interval a,Fractional n) => a -> a -> n
abs_dif i j = abs (i `interval` j)

sqr :: Num a => a -> a
sqr n = n * n

sqr_dif :: (Interval a,Fractional n) => a -> a -> n
sqr_dif i j = sqr (i `interval` j)

sqr_abs_dif :: (Interval a,Fractional n) => a -> a -> n
sqr_abs_dif i = sqr . abs_dif i

sqrt_abs_dif :: (Interval a,Floating n) => a -> a -> n
sqrt_abs_dif i = sqrt . abs_dif i

-- > city_block_metric (1,2) (3,5) == 2+3
city_block_metric :: (Interval a,Fractional n) => (a,a) -> (a,a) -> n
city_block_metric (x1,x2) (y1,y2) = abs_dif x1 y1 + abs_dif x2 y2

-- > euclidean_metric_2 (1,2) (3,5) == sqrt (4+9)
euclidean_metric_2 :: (Interval a,Floating n) => (a,a) -> (a,a) -> n
euclidean_metric_2 (x1,x2) (y1,y2) = sqrt (sqr_dif x1 y1 + sqr_dif x2 y2)

-- > euclidean_metric_l [1,2] [3,5] == sqrt (4+9)
-- > euclidean_metric_l [1,2,3] [2,4,6] == sqrt (1+4+9)
euclidean_metric_l :: (Interval a,Floating n) => [a] -> [a] -> n
euclidean_metric_l p = sqrt . sum . zipWith sqr_dif p

-- > cbrt 27 == 3
cbrt :: Floating a => a -> a
cbrt n = n ** (1/3)

nthrt :: Floating a => a -> a -> a
nthrt r n = n ** recip r

-- > minkowski_metric_2 1 (1,2) (3,5) == 5
-- > minkowski_metric_2 2 (1,2) (3,5) == sqrt (4+9)
-- > minkowski_metric_2 3 (1,2) (3,5) == cbrt (8+27)
minkowski_metric_2 :: Floating a => a -> (a,a) -> (a,a) -> a
minkowski_metric_2 n (x1,x2) (y1,y2) =
    ((abs (x1 - y1) ** n) + (abs (x2 - y2) ** n)) ** (1/n)

-- > minkowski_metric_l 2 [1,2,3] [2,4,6] == sqrt (1+4+9)
-- > minkowski_metric_l 3 [1,2,3] [2,4,6] == cbrt (1+8+27)
minkowski_metric_l :: Floating a => a -> [a] -> [a] -> a
minkowski_metric_l n p q =
    let f i j = abs (i - j) ** n
    in nthrt n (sum (zipWith f p q))

-- > d_dx [0,2,4,1,0] == [2,2,-3,-1]
-- > d_dx [2,3,0,4,1] == [1,-3,4,-3]
d_dx :: (Interval a,Fractional n) => [a] -> [n]
d_dx l = zipWith interval (tail l) l

-- > d_dx_abs [0,2,4,1,0] == [2,2,3,1]
-- > d_dx_abs [2,3,0,4,1] == [1,3,4,3]
d_dx_abs :: (Interval a,Fractional n) => [a] -> [n]
d_dx_abs = map abs . d_dx

ix_dif :: (Interval a,Fractional n) => [a] -> Int -> n
ix_dif x i = (x!!i) `interval` (x!!(i + 1))

-- > map (abs_ix_dif [0,2,4,1,0]) [0..3] == [2,2,3,1]
abs_ix_dif :: (Interval a,Fractional n) => [a] -> Int -> n
abs_ix_dif x i = abs (ix_dif x i)

-- > map (sqr_abs_ix_dif [0,2,4,1,0]) [0..3] == [4,4,9,1]
-- > map (sqr_abs_ix_dif [2,3,0,4,1]) [0..3] == [1,9,16,9]
sqr_abs_ix_dif :: (Interval a,Fractional n) => [a] -> Int -> n
sqr_abs_ix_dif x i = sqr (abs_ix_dif x i)

-- | Ordered linear magintude (no delta), p.300
--
-- > olm_no_delta' [0,2,4,1,0] [2,3,0,4,1] == 1.25
olm_no_delta' :: (Interval a,Fractional n) => [a] -> [a] -> n
olm_no_delta' p q =
    let r = zipWith (-) (d_dx_abs p) (d_dx_abs q)
        z = sum (map abs r)
    in z / (fromIntegral (length p) - 1)

{-
-- psi = Ψ
type PSI a = (a -> a -> a)

-- delta = Δ
type DELTA n a = ([n] -> Int -> a)

-- | Ordered linear magintude (generalised)
olm :: (Interval n,Fractional a,Enum a) => PSI a -> DELTA n a  -> [n] -> [n] -> a
olm psi delta m n =
    let l = length m
        l' = fromIntegral l - 1
        f i = psi (delta m i) (delta n i)
        maxint = 1
    in sum (map f [0..l-2]) / (l' * maxint)

-- > olm_no_delta [0,2,4,1,0] [2,3,0,4,1] == 1.25
olm_no_delta :: (Interval a,Fractional n) => [a] -> [a] -> n
olm_no_delta = olm abs_dif abs_ix_dif

second_order :: (Num n) => ([n] -> [n] -> t) -> [n] -> [n] -> t
second_order f p q = f (d_dx_abs (-) p) (d_dx_abs (-) q)

-- > olm_no_delta_second_order [0,2,4,1,0] [2,3,0,4,1] == 1.0
olm_no_delta_second_order :: (Enum a,Fractional a) => [a] -> [a] -> a
olm_no_delta_second_order = second_order olm_no_delta

-- > olm_no_delta_squared [0,2,4,1,0] [2,3,0,4,1] == sum (map sqrt [3,5,7,8]) / 4
olm_no_delta_squared :: (Enum a,Floating a) => [a] -> [a] -> a
olm_no_delta_squared = olm sqrt_abs_dif sqr_abs_ix_dif

-- p.301 erroneously gives this as sum (map sqrt [2,0,1]) / 3
-- > olm_no_delta_squared_second_order [0,2,4,1,0] [2,3,0,4,1] == sum (map sqrt [4,0,3]) / 3
olm_no_delta_squared_second_order :: (Enum a,Floating a) => [a] -> [a] -> a
olm_no_delta_squared_second_order = second_order olm_no_delta_squared

-- > direction_interval [5,9,3,2] == [LT,GT,GT]
-- > direction_interval [2,5,6,6] == [LT,LT,EQ]
direction_interval :: Ord i => [i] -> [Ordering]
direction_interval = d_dx (flip compare)

-- > ord_hist [LT,GT,GT] == (1,0,2)
ord_hist :: Integral t => [Ordering] -> (t,t,t)
ord_hist x =
    let h = T.histogram x
        f n = fromMaybe 0 (lookup n h)
    in (f LT,f EQ,f GT)

-- > direction_vector [5,9,3,2] == (1,0,2)
-- > direction_vector [2,5,6,6] == (2,1,0)
direction_vector :: Integral i => (Ord a) => [a] -> (i,i,i)
direction_vector = ord_hist . direction_interval

-- | Unordered linear direction, p.311 (Fig. 5)
--
-- > uld [5,9,3,2] [2,5,6,6] == 2/3
-- > uld [5,3,6,1,4] [3,6,1,4,2] == 0
uld :: (Integral n,Ord a) => [a] -> [a] -> Ratio n
uld m n =
    let (i,j,k) = direction_vector m
        (p,q,r) = direction_vector n
        z = (i + j + k) * 2
    in (abs_dif i p + abs_dif j q + abs_dif k r) % z

-- | Ordered linear direction, p.312
--
-- > direction_interval [5,3,6,1,4] == [GT,LT,GT,LT]
-- > direction_interval [3,6,1,4,2] == [LT,GT,LT,GT]
-- > old [5,3,6,1,4] [3,6,1,4,2] == 1
old :: (Ord i, Integral a) => [i] -> [i] -> Ratio a
old m n =
    let p = direction_interval m
        q = direction_interval n
        f i j = if i == j then 0 else 1
    in sum (zipWith f p q) % (genericLength m - 1)

-- | Ordered combinatorial direction, p.314
--
-- > ocd [5,9,3,2] [2,5,6,6] == 5/6
-- > ocd [5,3,6,1,4] [3,6,1,4,2] == 4/5
ocd :: (Ord a,Integral i) => [a] -> [a] -> Ratio i
ocd m n =
    let p = concat (T.half_matrix_f compare m)
        q = concat (T.half_matrix_f compare n)
        f i j = if i == j then 0 else 1
    in sum (zipWith f p q) % genericLength p

-- | Unordered combinatorial direction, p.314
--
-- > ucd [5,9,3,2] [2,5,6,6] == 5/6
-- > ucd [5,3,6,1,4] [3,6,1,4,2] == 0
-- > ucd [5,3,7,6] [2,1,2,1] == 1/2
-- > ucd [2,1,2,1] [8,3,5,4] == 1/3
-- > ucd [5,3,7,6] [8,3,5,4] == 1/3
ucd :: (Integral n,Ord a) => [a] -> [a] -> Ratio n
ucd m n =
    let (i,j,k) = ord_hist (concat (T.half_matrix_f compare m))
        (p,q,r) = ord_hist (concat (T.half_matrix_f compare n))
        z = (i + j + k) * 2
    in (abs_dif i p + abs_dif j q + abs_dif k r) % z
-}
