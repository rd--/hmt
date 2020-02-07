-- | Erv Wilson, archives <http://anaphoria.com/wilson.html>
module Music.Theory.Tuning.Wilson where

import Data.List {- base -}

import qualified Music.Theory.Tuning as T {- hmt -}

type R = Rational

-- * ZIG-ZAG

zz_seq_1 :: (Eq n,Num n) => Int -> (n,n) -> (n,n) -> [(n,n)]
zz_seq_1 k (p,q) (n,d) = if k == 0 then [(n,d)] else (n,d) : zz_seq_1 (k - 1) (p,q) (n+p,d+q)

-- > zz_next 3 [(0,1),(1,1)]
zz_next :: (Eq n, Num n) => Int -> [(n,n)] -> [(n,n)]
zz_next k p =
  case reverse p of
    i:j:_ -> zz_seq_1 k j i
    _ -> error "zz_next?"

zz_recur :: (Eq n, Num n) => [Int] -> [(n,n)] -> [[(n,n)]]
zz_recur k_seq p =
  case k_seq of
    [] -> []
    k:k_rem -> let r = zz_next k p in r : zz_recur k_rem r

-- > zz_seq [3,9,2,2,4,6,2,1,1,3]
-- > zz_seq [2,4,2,158]
-- > zz_seq [1,1,4,2,1,3,1,6,2]
zz_seq :: (Eq n, Num n) => [Int] -> [[(n, n)]]
zz_seq k_seq = zz_recur k_seq [(0,1),(1,1)]

-- * MOS

t2_sort :: Ord b => (b, b) -> (b, b)
t2_sort (i,j) = (min i j,max i j)

t2_sum :: Num a => (a, a) -> a
t2_sum (i,j) = i + j

-- > gen_coprime 12 == [1,5]
-- > gen_coprime 49 == [1..24] \\ [7,14,21]
gen_coprime :: Integral a => a -> [a]
gen_coprime x = filter (\y -> gcd y x == 1) [1 .. (x `div` 2)]

-- > mos_2 12 5 == (5,7)
mos_2 :: Num n => n -> n -> (n,n)
mos_2 p g = (g,p - g)

-- | Divide MOS, keeps retained value on same side
--
-- > mos_step (5,7) == (5,2)
-- > mos_step (5,2) == (3,2)
-- > mos_step (3,2) == (1,2)
mos_step :: (Ord a, Num a) => (a, a) -> (a, a)
mos_step (i,j) = if i < j then (i,j - i) else (i - j,j)

-- > mos_unfold (5,7)  == [(5,7),(5,2),(3,2),(1,2)]
-- > mos_unfold (41,17) == [(41,17),(24,17),(7,17),(7,10),(7,3),(4,3),(1,3),(1,2)]
mos_unfold :: (Ord b, Num b) => (b, b) -> [(b, b)]
mos_unfold x =
  let y = mos_step x
  in if t2_sum y == 3 then [x,y] else x : mos_unfold y

mos_verify :: Integral a => a -> a -> Bool
mos_verify p g =
  let x = if g > (p `div` 2) then p `mod` g else g
  in x `elem` gen_coprime p

-- > mos 12 5
mos :: (Ord b, Integral b) => b -> b -> [(b, b)]
mos p g = if mos_verify p g then mos_unfold (mos_2 p g) else error "mos?"

-- > mos_seq 12 5
-- > mos_seq 41 17
-- > mos_seq 49 27 -- 22
mos_seq :: (Ord b, Integral b) => b -> b -> [[b]]
mos_seq p g =
  let step_f (i,j) = concatMap (\x -> if x == i + j then [i,j] else [x])
      recur_f x l = if null x then [l] else l : recur_f (tail x) (step_f (head x) l)
      (i0,j0):r = mos p g
  in recur_f r [i0,j0]

mos_cell_pp :: (Integral i,Show i) => i -> String
mos_cell_pp x = let s = show x in s ++ genericReplicate (x - genericLength s) '-'

mos_row_pp :: (Integral i,Show i) => [i] -> String
mos_row_pp = concatMap mos_cell_pp

mos_tbl_pp :: (Integral i,Show i) => [[i]] -> [String]
mos_tbl_pp = map mos_row_pp

-- > mos_tbl_wr (mos_seq 49 27)
mos_tbl_wr :: (Integral i,Show i) => [[i]] -> IO ()
mos_tbl_wr = putStrLn . unlines . mos_tbl_pp

-- * MOS/LOG

mos_recip_seq :: Double -> [(Int,Double)]
mos_recip_seq x = let y = truncate x in (y,x) : mos_recip_seq (recip (x - fromIntegral y))

-- > take 9 (mos_log (5/4))
mos_log :: Double -> [(Int,Double)]
mos_log r = mos_recip_seq (recip (logBase 2 r))

-- > take 9 (mos_log_kseq 1.465571232) == [1,1,4,2,1,3,1,6,2]
mos_log_kseq :: Double -> [Int]
mos_log_kseq = map fst . mos_log

-- * TUNING

-- | (ratio,M3-steps)
type M3_GEN = (R,Int)

-- > map m3_gen_unfold [(3,4),(21/9,4),(15/9,4),(35/9,3),(21/5,4),(27/5,3)]
m3_gen_unfold :: M3_GEN -> [R]
m3_gen_unfold (r,n) = take n (iterate (* 3) r)

(^.) :: R -> Int -> R
(^.) = (^)

ew_xen456_9_gen :: [M3_GEN]
ew_xen456_9_gen =
  [(1/(3^.3),4)
  ,(1/(5*(3^.2)),3)
  ,(1/(7*3),3)
  ,(1/11,3)
  ,(5/(11*3),4)
  ,(7/11,2)]

{- | <http://anaphoria.com/xen456.pdf> P.9 {SCALA=NIL}

19-tone scale for the Clavichord-19 (1976)


> import qualified Music.Theory.Tuning.Scala as T {- hmt -}
> T.scl_find_ji (==) (ew_xen456_9 ++ [2])
-}
ew_xen456_9 :: [R]
ew_xen456_9 = (nub . sortOn T.fold_ratio_to_octave_err . concatMap m3_gen_unfold) ew_xen456_9_gen

{- | <http://anaphoria.com/Pelogflute.pdf> P.2 {SCALA=NIL}

9-tone Pelog cycle (1988)

> T.scl_find_ji (==) (ew_pf_2 ++ [2])
-}
ew_pf_2 :: Fractional n => [n]
ew_pf_2 = [1,16/15,64/55,5/4,4/3,16/11,8/5,128/75,20/11]

