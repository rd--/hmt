-- | Larry Polansky. \"Morphological Metrics\".
-- Journal of New Music Research, 25(4):289-368, 1996.
module Music.Theory.Metric.Polansky_1996 where

import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}

import qualified Music.Theory.Contour.Polansky_1992 as C {- hmt -}
import qualified Music.Theory.List as L {- hmt -}

-- | Distance function, ordinarily /n/ below is in 'Num', 'Fractional' or 'Real'.
type Interval a n = (a -> a -> n)

-- | 'fromIntegral' '.' '-'.
dif_i :: (Integral a,Num b) => a -> a -> b
dif_i i j = fromIntegral (i - j)

-- | 'realToFrac' '.' '-'.
dif_r :: (Real a,Fractional b) => a -> a -> b
dif_r i j = realToFrac (i - j)

-- | 'abs' '.' /f/.
abs_of :: Num n => Interval a n -> a -> a -> n
abs_of f i j = abs (i `f` j)

-- | Square.
sqr :: Num a => a -> a
sqr n = n * n

-- | 'sqr' '.' /f/.
sqr_of :: Num n => Interval a n -> a -> a -> n
sqr_of f i j = sqr (i `f` j)

-- | 'sqr' '.' 'abs' '.' /f/.
sqr_abs_of :: Num n => Interval a n -> a -> a -> n
sqr_abs_of f i = sqr . abs_of f i

-- | 'sqrt' '.' 'abs' '.' /f/.
sqrt_abs_of :: Floating c => Interval a c -> a -> a -> c
sqrt_abs_of f i = sqrt . abs_of f i

-- | City block metric, p.296
--
-- > city_block_metric (-) (1,2) (3,5) == 2+3
city_block_metric :: Num n => Interval a n -> (a,a) -> (a,a) -> n
city_block_metric f (x1,x2) (y1,y2) = abs_of f x1 y1 + abs_of f x2 y2

-- | Two-dimensional euclidean metric, p.297.
--
-- > euclidean_metric_2 (-) (1,2) (3,5) == sqrt (4+9)
euclidean_metric_2 :: Floating n => Interval a n -> (a,a) -> (a,a) -> n
euclidean_metric_2 f (x1,x2) (y1,y2) = sqrt (sqr_of f x1 y1 + sqr_of f x2 y2)

-- | /n/-dimensional euclidean metric
--
-- > euclidean_metric_l (-) [1,2] [3,5] == sqrt (4+9)
-- > euclidean_metric_l (-) [1,2,3] [2,4,6] == sqrt (1+4+9)
euclidean_metric_l :: Floating c => Interval b c -> [b] -> [b] -> c
euclidean_metric_l f p = sqrt . sum . zipWith (sqr_of f) p

-- | Cube root.
--
-- > map cbrt [1,8,27] == [1,2,3]
cbrt :: Floating a => a -> a
cbrt n = n ** (1/3)

-- | /n/-th root
--
-- > map (nthrt 4) [1,16,81] == [1,2,3]
nthrt :: Floating a => a -> a -> a
nthrt r n = n ** recip r

-- | Two-dimensional Minkowski metric, p.297
--
-- > minkowski_metric_2 (-) 1 (1,2) (3,5) == 5
-- > minkowski_metric_2 (-) 2 (1,2) (3,5) == sqrt (4+9)
-- > minkowski_metric_2 (-) 3 (1,2) (3,5) == cbrt (8+27)
minkowski_metric_2 :: Floating a => Interval t a -> a -> (t,t) -> (t,t) -> a
minkowski_metric_2 f n (x1,x2) (y1,y2) =
    ((abs (x1 `f` y1) ** n) + (abs (x2 `f` y2) ** n)) ** (1/n)

-- | /n/-dimensional Minkowski metric
--
-- > minkowski_metric_l (-) 2 [1,2,3] [2,4,6] == sqrt (1+4+9)
-- > minkowski_metric_l (-) 3 [1,2,3] [2,4,6] == cbrt (1+8+27)
minkowski_metric_l :: Floating a => Interval t a -> a -> [t] -> [t] -> a
minkowski_metric_l f n p q =
    let g i j = abs (i `f` j) ** n
    in nthrt n (sum (zipWith g p q))

-- | 'map' 'abs' '.' 'L.d_dx_by'.
--
-- > d_dx_abs (-) [0,2,4,1,0] == [2,2,3,1]
-- > d_dx_abs (-) [2,3,0,4,1] == [1,3,4,3]
d_dx_abs :: Num n => Interval a n -> [a] -> [n]
d_dx_abs f = map abs . L.d_dx_by f

-- | Ordered linear magnitude (no delta), p.300
--
-- > olm_no_delta' [0,2,4,1,0] [2,3,0,4,1] == 1.25
olm_no_delta' :: Fractional a => [a] -> [a] -> a
olm_no_delta' p q =
    let r = zipWith (-) (d_dx_abs (-) p) (d_dx_abs (-) q)
        z = sum (map abs r)
    in z / (fromIntegral (length p) - 1)

-- | Ordered linear magintude (general form) p.302
--
-- > olm_general (abs_of (-)) [0,2,4,1,0] [2,3,0,4,1] == 1.25
-- > olm_general (abs_of (-)) [1,5,12,2,9,6] [7,6,4,9,8,1] == 4.6
olm_general :: Fractional n => Interval a n -> [a] -> [a] -> n
olm_general f p q =
    let r = zipWith (-) (L.d_dx_by f p) (L.d_dx_by f q)
        z = sum (map abs r)
    in z / (fromIntegral (length p) - 1)

-- | 'Delta' (Δ) determines an interval given a sequence and an index.
type Delta n a = ([n] -> Int -> a)

-- | /f/ at indices /i/ and /i+1/ of /x/.
--
-- > map (ix_dif (-) [0,1,3,6,10]) [0..3] == [-1,-2,-3,-4]
ix_dif :: Interval a t -> Delta a t
ix_dif f x i = (x !! i) `f` (x !! (i + 1))

-- | 'abs' '.' 'ix_dif'
--
-- > map (abs_ix_dif (-) [0,2,4,1,0]) [0..3] == [2,2,3,1]
abs_ix_dif :: Num n => Interval a n -> Delta a n
abs_ix_dif f x i = abs (ix_dif f x i)

-- | 'sqr' '.' 'abs_ix_dif'
--
-- > map (sqr_abs_ix_dif (-) [0,2,4,1,0]) [0..3] == [4,4,9,1]
-- > map (sqr_abs_ix_dif (-) [2,3,0,4,1]) [0..3] == [1,9,16,9]
sqr_abs_ix_dif :: Num n => Interval a n -> Delta a n
sqr_abs_ix_dif f x i = sqr (abs_ix_dif f x i)

-- | 'Psi' (Ψ) joins 'Delta' equivalent intervals from morphologies /m/ and /n/.
type Psi a = (a -> a -> a)

-- | Ordered linear magintude (generalised-interval form) p.305
--
-- > olm (abs_of dif_r) (abs_ix_dif dif_r) (const 1) [1,5,12,2,9,6] [7,6,4,9,8,1] == 4.6
-- > olm (abs_of dif_r) (abs_ix_dif dif_r) maximum [1,5,12,2,9,6] [7,6,4,9,8,1] == 0.46
olm :: Fractional a => Psi a -> Delta n a  -> ([a] -> a) -> [n] -> [n] -> a
olm psi delta maxint m n =
    let l = length m
        l' = fromIntegral l - 1
        k = [0..l-2]
        m' = map (delta m) k
        n' = map (delta n) k
    in sum (zipWith psi m' n') / (l' * maxint (m' ++ n'))

-- > olm_no_delta [0,2,4,1,0] [2,3,0,4,1] == 1.25
-- > olm_no_delta [1,6,2,5,11] [3,15,13,2,9] == 4.5
olm_no_delta :: (Real a,Real n,Fractional n) => [a] -> [a] -> n
olm_no_delta = olm (abs_of dif_r) (abs_ix_dif dif_r) (const 1)

-- > olm_no_delta_squared [0,2,4,1,0] [2,3,0,4,1] == sum (map sqrt [3,5,7,8]) / 4
olm_no_delta_squared :: Floating a => [a] -> [a] -> a
olm_no_delta_squared = olm (sqrt_abs_of (-)) (sqr_abs_ix_dif (-)) (const 1)

second_order :: (Num n) => ([n] -> [n] -> t) -> [n] -> [n] -> t
second_order f p q = f (d_dx_abs (-) p) (d_dx_abs (-) q)

-- > olm_no_delta_second_order [0,2,4,1,0] [2,3,0,4,1] == 1.0
olm_no_delta_second_order :: (Real a,Fractional a) => [a] -> [a] -> a
olm_no_delta_second_order = second_order olm_no_delta

-- p.301 erroneously gives this as sum (map sqrt [2,0,1]) / 3
-- > olm_no_delta_squared_second_order [0,2,4,1,0] [2,3,0,4,1] == sum (map sqrt [4,0,3]) / 3
olm_no_delta_squared_second_order :: Floating a => [a] -> [a] -> a
olm_no_delta_squared_second_order = second_order olm_no_delta_squared

-- | Second order binomial coefficient, p.307
--
-- > map second_order_binonial_coefficient [2..10] == [1,3,6,10,15,21,28,36,45]
second_order_binonial_coefficient :: Fractional a => a -> a
second_order_binonial_coefficient n = ((n * n) - n) / 2

-- | 'L.d_dx_by' of 'flip' 'compare'.
--
-- > direction_interval [5,9,3,2] == [LT,GT,GT]
-- > direction_interval [2,5,6,6] == [LT,LT,EQ]
direction_interval :: Ord i => [i] -> [Ordering]
direction_interval = L.d_dx_by (flip compare)

-- | Histogram of list of 'Ordering's.
--
-- > ord_hist [LT,GT,GT] == (1,0,2)
ord_hist :: Integral t => [Ordering] -> (t,t,t)
ord_hist x =
    let h = L.generic_histogram x
        f n = fromMaybe 0 (lookup n h)
    in (f LT,f EQ,f GT)

-- | Histogram of /directions/ of adjacent elements, p.312.
--
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
    in (abs_of (-) i p + abs_of (-) j q + abs_of (-) k r) % z

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
    let p = concat (C.half_matrix_f compare m)
        q = concat (C.half_matrix_f compare n)
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
    let (i,j,k) = ord_hist (concat (C.half_matrix_f compare m))
        (p,q,r) = ord_hist (concat (C.half_matrix_f compare n))
        z = (i + j + k) * 2
    in (abs_of (-) i p + abs_of (-) j q + abs_of (-) k r) % z

-- | 'C.half_matrix_f', Fig.9, p.318
--
-- > let r = [[2,3,1,4],[1,3,6],[4,7],[3]]
-- > combinatorial_magnitude_matrix (abs_of (-)) [5,3,2,6,9] == r
combinatorial_magnitude_matrix :: Interval a n -> [a] -> [[n]]
combinatorial_magnitude_matrix = C.half_matrix_f

-- | Unordered linear magnitude (simplified), p.320-321
--
-- > let r = abs (sum [5,4,3,6] - sum [12,2,11,7]) / 4
-- > ulm_simplified (abs_of (-)) [1,6,2,5,11] [3,15,13,2,9] == r
--
-- > ulm_simplified (abs_of (-)) [1,5,12,2,9,6] [7,6,4,9,8,1] == 3
ulm_simplified :: Fractional n => Interval a n -> [a] -> [a] -> n
ulm_simplified f p q =
    let g = abs . sum . L.d_dx_by f
    in abs (g p - g q) / fromIntegral (length p - 1)

ocm_zcm :: Fractional n => Interval a n -> [a] -> [a] -> (n, n, [n])
ocm_zcm f p q =
    let p' = concat (C.half_matrix_f f p)
        q' = concat (C.half_matrix_f f q)
        r = zipWith (-) p' q'
        z = sum (map abs r)
        c = second_order_binonial_coefficient (fromIntegral (length p))
        m = p' ++ q'
    in (z,c,m)

-- | Ordered combinatorial magnitude (OCM), p.323
--
-- > ocm (abs_of (-)) [1,6,2,5,11] [3,15,13,2,9] == 5.2
-- > ocm (abs_of (-)) [1,5,12,2,9,6] [7,6,4,9,8,1] == 3.6
ocm :: Fractional n => Interval a n -> [a] -> [a] -> n
ocm f p q =
    let (z,c,_) = ocm_zcm f p q
    in z / c

-- | Ordered combinatorial magnitude (OCM), p.323
--
-- > ocm_absolute_scaled (abs_of (-)) [1,6,2,5,11] [3,15,13,2,9] == 0.4
-- > ocm_absolute_scaled (abs_of (-)) [1,5,12,2,9,6] [7,6,4,9,8,1] == 54/(15*11)
ocm_absolute_scaled :: (Ord n,Fractional n) => Interval a n -> [a] -> [a] -> n
ocm_absolute_scaled f p q =
    let (z,c,m) = ocm_zcm f p q
    in z / (c * maximum m)
