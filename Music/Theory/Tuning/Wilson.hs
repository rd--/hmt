-- | Erv Wilson, archives <http://anaphoria.com/wilson.html>
module Music.Theory.Tuning.Wilson where

import Control.Monad {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}
import Safe {- safe -}
import Text.Printf {- base -}

import qualified Music.Theory.Array.Text as T {- hmt -}
import qualified Music.Theory.Graph.Dot as T {- hmt -}
import qualified Music.Theory.Graph.Type as T {- hmt -}
import qualified Music.Theory.Interval.Barlow_1987 as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Math as T {- hmt -}
import qualified Music.Theory.Math.Convert as T {- hmt -}
import qualified Music.Theory.Math.OEIS as T {- hmt -}
import qualified Music.Theory.Math.Prime as T {- hmt -}
import qualified Music.Theory.Set.List as T {- hmt -}
import qualified Music.Theory.Show as T {- hmt -}
import qualified Music.Theory.Tuning as T {- hmt -}
import qualified Music.Theory.Tuning.Scala as T {- hmt -}
import qualified Music.Theory.Tuple as T {- hmt -}

-- * GEOM

-- | (x,y) co-ordinate
type Pt n = (n,n)

pt_add :: Num n => Pt n -> Pt n -> Pt n
pt_add (p,q) (i,j) = (p + i,q + j)

pt_sum :: Num n => [Pt n] -> Pt n
pt_sum = foldl pt_add (0,0)

pt_scale :: Num n => n -> Pt n -> Pt n
pt_scale m (x,y) = (m * x,m * y)

-- > map (flip pt_scale_i (20,0)) [0,1,-1] == [(0,0),(20,0),(-20,-0)]
pt_scale_i :: Num n => Int -> Pt n -> Pt n
pt_scale_i m = pt_scale (fromIntegral m)

-- | Normalise set of points to lie in (-1,-1) - (1,1)
--
-- > pt_set_normalise kg_lc_std
pt_set_normalise :: (Fractional n,Ord n) => [Pt n] -> [Pt n]
pt_set_normalise x = let z = maximum (map (uncurry max) x) in map (T.t2_map (* (recip z))) x

-- * LATTICE CO-ORD

-- | /k/-unit co-ordinates for /k/-lattice.
type LC n = [Pt n]

-- | Erv Wilson standard lattice, unit co-ordinates for 5-dimensions, ie. [3,5,7,11,13]
--
-- <http://anaphoria.com/wilsontreasure.html>
ew_lc_std :: Num n => LC n
ew_lc_std = [(20,0),(0,20),(4,3),(-3,4),(-1,2)]

-- | Kraig Grady standard lattice, unit co-ordinates for 5-dimensions, ie. [3,5,7,11,13]
--
-- <http://anaphoria.com/wilsontreasure.html>
kg_lc_std :: Num n => LC n
kg_lc_std = [(40,0),(0,40),(13,11),(-14,18),(-8,4)]

-- | Erv Wilson tetradic lattice, used especially when working with hexanies or 7 limit tunings
--
-- <http://anaphoria.com/wilsontreasure.html>
ew_lc_tetradic :: Num n => LC n
ew_lc_tetradic = [(-4,-2),(6,1),(5,-2)]

-- | Resolve POS against LC to Pt
lc_pos_to_pt :: (Fractional n, Ord n) => LC n -> POS -> Pt n
lc_pos_to_pt lc x = pt_sum (zipWith pt_scale_i x (pt_set_normalise lc))

-- * LAT

-- | A discrete /k/-lattice is described by a sequence of /k/-factors.
--   LAT values are ordinarily though not necessarily primes.
type LAT = [Integer]

-- | Positions in a /k/-lattice are given as a /k/-list of steps.
type POS = [Int]

-- | White-space pretty printer for POS.
--
-- > pos_pp_ws [0,-2,1] == "  0 -2  1"
pos_pp_ws :: POS -> String
pos_pp_ws = let f x = printf "%3d" x in concatMap f

-- | Given LAT [X,Y,Z..] and POS [x,y,z..], calculate the indicated ratio.
--
-- > lat_res [3,5] [-5,2] == (5 * 5) / (3 * 3 * 3 * 3 * 3)
lat_res :: LAT -> POS -> Rational
lat_res p q =
  let f i j = case compare j 0 of
                GT -> (i ^ T.int_to_integer j) % 1
                EQ -> 1
                LT -> 1 % (i ^ abs (T.int_to_integer j))
  in product (zipWith f p q)

-- * RAT (n,d)

-- | Ratio given as (/n/,/d/)
type RAT = (Integer,Integer)

-- | Remove all octaves from /n/ and /d/.
rat_rem_oct :: RAT -> RAT
rat_rem_oct = T.bimap1 (product . filter (/= 2)) . T.rat_prime_factors

-- | Lift 'RAT' function to 'Rational'.
rat_lift_1 :: (RAT -> RAT) -> Rational -> Rational
rat_lift_1 f = uncurry (%) . f . T.rational_nd

rat_to_ratio :: RAT -> Rational
rat_to_ratio (n,d) = n % d

-- | Mediant, ie. n1+n2/d1+d2
--
-- > rat_mediant (0,1) (1,2) == (1,3)
rat_mediant :: RAT -> RAT -> RAT
rat_mediant (n1,d1) (n2,d2) = (n1 + n2,d1 + d2)

rat_pp :: RAT -> String
rat_pp (n,d) = concat [show n,"/",show d]

-- * Rational

-- | Lifted 'rat_rem_oct'.
--
-- > map ew_r_rem_oct [256/243,7/5,1/7] == [1/243,7/5,1/7]
r_rem_oct :: Rational -> Rational
r_rem_oct = rat_lift_1 rat_rem_oct

-- | Assert that /n/ is in [1,2).
r_verify_oct :: Rational -> Rational
r_verify_oct i = if i >= 1 && i < 2 then i else error (show ("r_verify_oct?",i))

-- | Find limit of set of ratios, ie. largest factor in either numerator or denominator.
--
-- > r_seq_limit [1] == 1
r_seq_limit :: [Rational] -> Integer
r_seq_limit = maximum . map T.rational_prime_limit

-- * Table

-- > map (rat_fact_lm 11) [3,5,7,11] == [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]
rat_fact_lm :: Integer -> Rational -> POS
rat_fact_lm lm = tail . T.rat_prime_factors_t (fromMaybe 1 (T.prime_k lm) + 1) . T.rational_nd

tbl_txt :: Integer -> [Rational] -> [[String]]
tbl_txt lm_z rs =
  let lm = r_seq_limit rs
      scl = map (rat_fact_lm lm) rs
      cs = map (T.ratio_to_cents . T.fold_ratio_to_octave_err) rs
      hs = map (T.harmonicity_r T.barlow) rs :: [Double]
      f (k,x,r,c,h) = [show k
                      ,if lm <= lm_z then pos_pp_ws x else "..."
                      ,T.ratio_pp r
                      ,T.real_pp 2 c
                      ,T.real_pp_unicode 2 h]
  in map (intersperse "=" . f) (zip5 [0::Int ..] scl rs cs hs)

-- > tbl_wr [1,7/6,5/4,4/3,3/2]
tbl_wr :: [Rational] -> IO ()
tbl_wr = putStr . unlines . T.table_pp (False,True,False," ",False) . tbl_txt 31

-- * Graph

-- | (maybe-lc,gr-attr,vertex-pp)
type EW_GR_OPT = (Maybe (LC Rational),[T.DOT_META_ATTR],Rational -> String)

ew_gr_opt_pos :: EW_GR_OPT -> Bool
ew_gr_opt_pos (lc_m,_,_) = isJust lc_m

ew_gr_r_pos :: LC Rational -> Rational -> T.DOT_ATTR
ew_gr_r_pos lc = T.g_pos_attr 160 . lc_pos_to_pt lc . Safe.tailDef [] . T.rational_prime_factors_l

ew_gr_udot :: EW_GR_OPT -> T.LBL Rational () -> [String]
ew_gr_udot (lc_m,attr,v_pp) =
  let (e,p_f) = case lc_m of
                  Nothing -> ("sfdp",const Nothing)
                  Just lc -> ("neato",Just . ew_gr_r_pos lc)
  in T.lbl_to_udot
     ([("graph:layout",e),("node:shape","plain")] ++ attr) -- ("graph:K","0.6") ("edge:len","1.0")
     (\v -> T.mcons (p_f v) [("label",v_pp v)]
     ,\_ -> [])

ew_gr_udot_wr :: EW_GR_OPT -> FilePath -> T.LBL Rational () -> IO ()
ew_gr_udot_wr opt fn = writeFile fn . unlines . ew_gr_udot opt

ew_gr_udot_wr_svg :: EW_GR_OPT -> FilePath -> T.LBL Rational () -> IO ()
ew_gr_udot_wr_svg opt fn gr = do
  ew_gr_udot_wr opt fn gr
  void (T.dot_to_svg (if ew_gr_opt_pos opt then ["-n"] else []) fn)

-- * ZIG-ZAG

zz_seq_1 :: (Eq n,Num n) => Int -> (n,n) -> (n,n) -> [(n,n)]
zz_seq_1 k (p,q) (n,d) = if k == 0 then [(n,d)] else (n,d) : zz_seq_1 (k - 1) (p,q) (n+p,d+q)

-- > zz_next 3 [(0,1),(1,1)] == [(1,1),(1,2),(1,3),(1,4)]
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
  in if T.t2_sum y == 3 then [x,y] else x : mos_unfold y

mos_verify :: Integral a => a -> a -> Bool
mos_verify p g =
  let x = if g > (p `div` 2) then p `mod` g else g
  in x `elem` gen_coprime p

-- > mos 12 5 == [(5,7),(5,2),(3,2),(1,2)]
mos :: (Ord b, Integral b) => b -> b -> [(b, b)]
mos p g = if mos_verify p g then mos_unfold (mos_2 p g) else error "mos?"

-- > mos_seq 12 5 == [[5,7],[5,5,2],[3,2,3,2,2],[1,2,2,1,2,2,2]]
-- > mos_seq 41 17 !! 4 == [3,3,4,3,4,3,3,4,3,4,3,4]
-- > map length (mos_seq 49 27) == [2,3,5,7,9,11,20,29]
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

-- > take 3 (mos_log (5/4)) == [(3,3.10628371950539),(9,9.408778735385603),(2,2.4463112031908785)]
mos_log :: Double -> [(Int,Double)]
mos_log r = mos_recip_seq (recip (logBase 2 r))

-- > take 9 (mos_log_kseq 1.465571232) == [1,1,4,2,1,3,1,6,2]
mos_log_kseq :: Double -> [Int]
mos_log_kseq = map fst . mos_log

-- * STERN-BROCOT TREE

data SBT_DIV = NIL | LHS | RHS deriving (Show)
type SBT_NODE = (SBT_DIV,RAT,RAT,RAT)

sbt_step :: SBT_NODE -> [SBT_NODE]
sbt_step (_,l,m,r) = [(LHS,l,rat_mediant l m, m),(RHS,m,rat_mediant m r,r)]

-- sbt = stern-brocot tree
sbt_root :: SBT_NODE
sbt_root = (NIL,(0,1),(1,1),(1,0))

sbt_half :: SBT_NODE
sbt_half = (NIL,(0,1),(1,2),(1,1))

-- > sbt_from sbt_root
sbt_from :: SBT_NODE -> [[SBT_NODE]]
sbt_from = iterate (concatMap sbt_step) . return

sbt_k_from :: Int -> SBT_NODE -> [[SBT_NODE]]
sbt_k_from k = take k . sbt_from

sbt_node_to_edge :: SBT_NODE -> String
sbt_node_to_edge (dv,l,m,r) =
  let edge_pp p q = printf "\"%s\" -- \"%s\"" (rat_pp p) (rat_pp q)
  in case dv of
       NIL -> ""
       LHS -> edge_pp r m
       RHS -> edge_pp l m

sbt_node_elem :: SBT_NODE -> [RAT]
sbt_node_elem (dv,l,m,r) =
  case dv of
    NIL -> [l,m,r]
    _ -> [m]

sbt_dot :: [SBT_NODE] -> [String]
sbt_dot n =
  let e = map sbt_node_to_edge n
  in concat [["graph {","node [shape=plain]"],e,["}"]]

-- * M3-GEN

-- | (ratio,M3-steps)
type M3_GEN = (Rational,Int)

-- > map m3_gen_unfold [(3,4),(21/9,4),(15/9,4),(35/9,3),(21/5,4),(27/5,3)]
m3_gen_unfold :: M3_GEN -> [Rational]
m3_gen_unfold (r,n) = take n (iterate (* 3) r)

(^.) :: Rational -> Int -> Rational
(^.) = (^)

r_normalise :: [Rational] -> [Rational]
r_normalise = nub . sortOn T.fold_ratio_to_octave_err

m3_gen_to_r :: [M3_GEN] -> [Rational]
m3_gen_to_r = r_normalise . concatMap m3_gen_unfold

-- * SCALA

r_to_scale :: String -> String -> [Rational] -> T.Scale
r_to_scale nm dsc r =
  let r' = map T.fold_ratio_to_octave_err (tail r) ++ [2]
  in if r !! 0 /= 1 || not (T.is_ascending r')
     then error "r_to_scale?"
     else (nm,dsc,length r,map Right r')

ew_scl_find_r :: [Rational] -> IO [String]
ew_scl_find_r r =
  let set_eq x y = sort x == sort y
  in if head r /= 1
     then error "ew_scl_find_r?"
     else fmap (map T.scale_name) (T.scl_find_ji set_eq (map T.fold_ratio_to_octave_err r ++ [2]))

-- * <http://anaphoria.com/1-3-5-7-9Genus.pdf>

ew_1357_3_gen :: [M3_GEN]
ew_1357_3_gen = [(3,4),(21/9,4),(15/9,4),(35/9,3),(21/5,4),(27/5,3)]

{- | P.3 7-limit {SCALA=NIL}

> ew_scl_find_r (1 : ew_1357_3_r)
-}
ew_1357_3_r :: [Rational]
ew_1357_3_r = r_normalise (concatMap m3_gen_unfold ew_1357_3_gen)

ew_1357_3_scl :: T.Scale
ew_1357_3_scl = r_to_scale "ew_1357_3" "EW, 1-3-5-7-9Genus.pdf, P.3" (1 : ew_1357_3_r)

-- * <http://anaphoria.com/earlylattices12.pdf>

{- | P.7 11-limit {SCALA=NIL}

> ew_scl_find_r ew_el12_7_r
-}
ew_el12_7_r :: [Rational]
ew_el12_7_r = [1,5/(7*11),1/7,7*11,7*11*11/5,11,5/7,1/11,7*11*11,1/(7*11),11*11,7*11/5]

ew_el12_7_scl :: T.Scale
ew_el12_7_scl = r_to_scale "ew_el12_7" "EW, earlylattices12.pdf, P.7" ew_el12_7_r

{- | P.9 7-limit {SCALA=wilson_class}

> ew_scl_find_r ew_el12_9_r
-}
ew_el12_9_r :: [Rational]
ew_el12_9_r = [1,5*5/3,7/(5*5),7/3,5,1/3,7/5,5*7/3,1/5,5/3,7,7/(3*5)]

--ew_el12_9_scl :: T.Scale
--ew_el12_9_scl = r_to_scale "ew_el12_9" "EW, earlylattices12.pdf, P.9" ew_el12_9_r

{- | P.12 11-limit {SCALA=NIL}

> ew_scl_find_r ew_el12_12_r
-}
ew_el12_12_r :: [Rational]
ew_el12_12_r = [1,3*3*5/11,3/11,7/3,5,7/11,3*5/11,5*7/3,7/(3*3),5*7/11,7/(3*11),3*5]

ew_el12_12_scl :: T.Scale
ew_el12_12_scl = r_to_scale "ew_el12_12" "EW, earlylattices12.pdf, P.12" ew_el12_12_r

-- * <http://anaphoria.com/earlylattices22.pdf>

{- | P.2 11-limit {SCALA=wilson_l4}

> ew_scl_find_r ew_el22_2_r
-}
ew_el22_2_r :: [Rational]
ew_el22_2_r =
  [1,7*7/3,3*7/5,5/(3*3),1/7,7/3,3/5,5,5*7/(3*3*3),1/3,7*7/(3*3)
  ,7/5,5*7/3,3,7/(3*3),1/5,5/3,3/7,7,3*3/5,7/(3*5),5*7/(3*3)]

{- | P.3 11-limit {SCALA=wilson_l5}

> ew_scl_find_r ew_el22_3_r
-}
ew_el22_3_r :: [Rational]
ew_el22_3_r =
  [1,7*7/3,7*11/(3*3),3/11,1/7,7/3,3/5,5,7/11,1/3,7*7/(3*3)
  ,7/5,5*7/3,3,7/(3*3),1/5,5/3,3/7,7,11/3,7/(3*5),5*7/(3*3)]

{- | P.4 11-limit {SCALA=wilson_l3}

> ew_scl_find_r ew_el22_4_r
-}
ew_el22_4_r :: [Rational]
ew_el22_4_r =
  [1,3*11,3*7/5,5*7,3*3,7/3,3/5,5,7/11,3*7,11
  ,7/5,5*7/3,3,7/(3*3),1/5,3*5*7,3*3*3,7,3*3/5,3*5,3*7/11]

{- | P.5 11-limit {SCALA=wilson_l1}

> ew_scl_find_r ew_el22_5_r
-}
ew_el22_5_r :: [Rational]
ew_el22_5_r =
  [1,3*11,3*7/5,5*7,3*3,7/3,7*11,5,3*5*11,3*7,11
  ,7/5,3*7*11/5,3,3*3*11,7*11/3,3*11/5,5*11,7,3*7*11,3*5,7*11/5]

{- | P.6 11-limit {SCALA=wilson_l2}

> ew_scl_find_r ew_el22_6_r
-}
ew_el22_6_r :: [Rational]
ew_el22_6_r =
  [1,7*7/3,7*11/(3*3),11/5,3*3,7/3,7*11,5,7*11/(3*5),1/3,11
  ,7*11/(3*3*3),5*7/3,3,11/7,7*11/3,5/3,7*11/(3*3*5),7,11/3,3*5,7*11/5]

-- * <http://anaphoria.com/diamond.pdf>

ew_diamond_mk :: [Integer] -> [Rational]
ew_diamond_mk u = r_normalise [x % y | x <- u, y <- u]

-- > m3_gen_to_r ew_diamond_12_gen == ew_diamond_12_r
ew_diamond_12_gen :: [M3_GEN]
ew_diamond_12_gen =
  [(1/(3^.2),5),(5/(3^.2),3),(7/(3^.2),3),(11/(3^.2),3)
  ,(1/5,3),(1/7,3),(1/11,3)
  ,(5/7,1),(5/11,1),(7/5,1),(7/11,1),(11/5,1),(11/7,1)]

{- | P.7 & P.12 11-limit {SCALA=partch_29}

1,3,5,7,9,11 diamond

> ew_scl_find_r ew_diamond_12_r -- partch_29
-}
ew_diamond_12_r :: [Rational]
ew_diamond_12_r = ew_diamond_mk [1,3,5,7,9,11]

{- | P.10 & P.13 13-limit {SCALA=novaro15}

1,3,5,7,9,11,13,15 diamond

> ew_scl_find_r ew_diamond_13_r -- novaro15
-}
ew_diamond_13_r :: [Rational]
ew_diamond_13_r = ew_diamond_mk [1,3,5,7,9,11,13,15]

-- * <http://anaphoria.com/hel.pdf>

hel_r_asc :: (Integer,Integer) -> [Rational]
hel_r_asc (n,d) = n%d : hel_r_asc (n+1,d+1)

type HEL = ([Rational],[Rational])

-- | P.6
hel_1_i :: HEL
hel_1_i =
  let i = take 6 (hel_r_asc (7,6))
  in (take 5 i,take 5 (T.rotate_left 2 i))

-- | P.6
hel_2_i :: HEL
hel_2_i =
  let i = take 10 (hel_r_asc (9,8))
  in (take 8 (T.rotate_left 3 (tail i))
     ,take 7 i)

-- | P.10
hel_3_i :: HEL
hel_3_i =
  let i = take 16 (hel_r_asc (15,14))
  in (take 13 (T.rotate_left 6 (take 14 i)),take 14 (tail i))

hel_r :: HEL -> [[Rational]]
hel_r (p,q) =
  let i_to_r = scanl (*) 1
  in [i_to_r p,i_to_r q,r_normalise (concat [i_to_r p,i_to_r q])]

{- | P.12 {SCALA=NIL}

22-tone 23-limit Evangalina tuning (2001)

> ew_scl_find_r ew_hel_12_r
-}
ew_hel_12_r :: [Rational]
ew_hel_12_r =
  [1,3*3*3*5,13/3,5/(3*3),3*3,7/3,11/(3*3),5,3*3*3*3,1/3,11
  ,3*3*5,17/3,3,3*3*3*3*5,13,5/3,3*3*3,7,11/3,3*5,23/3]

ew_hel_12_scl :: T.Scale
ew_hel_12_scl = r_to_scale "ew_hel_12" "EW, hel.pdf, P.12" ew_hel_12_r

-- * <http://anaphoria.com/HexanyStellatesExpansions.pdf>

-- > she_div "ABCD" == [["BCD","A"],["ACD","B"],["ABD","C"],["ABC","D"]]
she_div :: Eq a => [a] -> [[[a]]]
she_div x =
  let f = (== [1,length x - 1]) . sort . map length
  in map (reverse . sortOn length) (filter f (T.partitions x))

-- > she_div_r [1,3,5,7] == [105,35/3,21/5,15/7]
she_div_r :: [Rational] -> [Rational]
she_div_r =
  let f x =
        case x of
          [[a,b,c],[d]] -> (a * b * c) / d
          _ -> error "she_div?"
  in map f . she_div

-- > she_mul_r [1,3,5,7] == [1,3,5,7,9,15,21,25,35,49]
she_mul_r :: [Rational] -> [Rational]
she_mul_r r = [(x * y) | x <- r,y <- r,x <= y]

{- | she = Stellate Hexany Expansions, P.10 {SCALA=stelhex1,stelhex2,stelhex5,stelhex6}

> she [1,3,5,7] == [1,21/20,15/14,35/32,9/8,5/4,21/16,35/24,3/2,49/32,25/16,105/64,7/4,15/8]
> mapM (ew_scl_find_r . she) [[1,3,5,7],[1,3,5,9],[1,3,7,9],[1,3,5,11]]
> ew_scl_find_r (she [1,(5*7)/(3*3),1/(3 * 5),1/3]) -- NIL
-}
she :: [Rational] -> [Rational]
she r = nub (sort (map T.fold_ratio_to_octave_err (she_mul_r r ++ she_div_r r)))

-- * <http://anaphoria.com/meru.pdf>

-- > map (every_nth "abcdef") [1..3] == ["abcdef","ace","ad"]
every_nth :: [t] -> Int -> [t]
every_nth l k =
  case l of
    [] -> []
    x:_ -> x : every_nth (drop k l) k

meru :: Num n => [[n]]
meru =
  let f xs = zipWith (+) ([0] ++ xs) (xs ++ [0])
  in iterate f [1]

-- > meru_k 13
meru_k :: Num n => Int -> [[n]]
meru_k k = take k meru

-- > map (sum . meru_1) [1 .. 13] == [1,1,2,3,5,8,13,21,34,55,89,144,233]
meru_1 :: Num n => Int -> [n]
meru_1 k = zipWith (\x l -> atDef 0 l x) [0..] (reverse (meru_k k))

-- > take 13 meru_1_direct == [1,1,2,3,5,8,13,21,34,55,89,144,233]
meru_1_direct :: Num n => [n]
meru_1_direct = tail T.a000045

-- | Meru 2 = META-PELOG
--
-- > map (sum . meru_2) [1 .. 14] == [1,1,1,2,3,4,6,9,13,19,28,41,60,88]
meru_2 :: Num n => Int -> [n]
meru_2 k = zipWith (\x l -> atDef 0 l x) [0..] (every_nth (reverse (meru_k k)) 2)

-- > take 14 meru_2_direct == [1,1,1,2,3,4,6,9,13,19,28,41,60,88]
meru_2_direct :: Num n => [n]
meru_2_direct = T.a000930

-- | meru_3 = META-SLENDRO
meru_3 :: Num n => Int -> [[n]]
meru_3 k =
  let f t = zipWith (\x l -> atDef 0 l x) [0,2..] t
      t0 = reverse (meru_k k)
      t1 = map tail t0
  in [f t0,f t1]

-- > map sum (meru_3_seq 13) == [1,0,1,1,1,2,2,3,4,5,7,9,12,16,21,28,37,49,65,86,114,151,200,265,351,465]
meru_3_seq :: Num n => Int -> [[n]]
meru_3_seq k = concatMap meru_3 [1 .. k]

-- > take 26 meru_3_direct == [1,0,1,1,1,2,2,3,4,5,7,9,12,16,21,28,37,49,65,86,114,151,200,265,351,465]
meru_3_direct :: Num n => [n]
meru_3_direct = drop 3 T.a000931

-- > map (sum . meru_4) [1 .. 13] == [1,1,1,1,2,3,4,5,7,10,14,19,26]
meru_4 :: Num n => Int -> [n]
meru_4 k = zipWith (\x l -> atDef 0 l x) [0..] (every_nth (reverse (meru_k k)) 3)

-- > take 31 meru_4_direct == map (sum . meru_4) [1 .. 31]
meru_4_direct :: Num n => [n]
meru_4_direct = tail T.a003269

-- > map meru_5 [1..4]
meru_5 :: Num n => Int -> [[n]]
meru_5 k =
  let f t = zipWith (\x l -> atDef 0 l x) [0,3..] t
      t0 = reverse (meru_k k)
  in map (\n -> f (map (drop n) t0)) [0 .. 2]

-- > map sum (meru_5_seq 13)
meru_5_seq :: Num n => Int -> [[n]]
meru_5_seq k = concatMap meru_5 [1 .. k]

-- > take 39 meru_5_direct == map sum (meru_5_seq 13)
meru_5_direct :: Num n => [n]
meru_5_direct = T.a017817

-- > map (sum . meru_6) [1 .. 21] == [1,1,1,1,1,2,3,4,5,6,8,11,15,20,26,34,45,60,80,106,140]
meru_6 :: Num n => Int -> [n]
meru_6 k = zipWith (\x l -> atDef 0 l x) [0..] (every_nth (reverse (meru_k k)) 4)

-- > take 21 meru_6_direct == map (sum . meru_6) [1 .. 21]
meru_6_direct :: Num n => [n]
meru_6_direct = T.a003520

-- > take 26 meru_7_direct == [0,1,0,1,0,1,1,1,2,1,3,2,4,4,5,7,7,11,11,16,18,23,29,34,45,52]
meru_7_direct :: Num n => [n]
meru_7_direct = T.a001687

-- * <http://anaphoria.com/mos.pdf>

{- | P.13, tanabe {SCALA=chin_7}

> ew_scl_find_r ew_mos_13_tanabe_r
-}
ew_mos_13_tanabe_r :: [Rational]
ew_mos_13_tanabe_r = [1,9/8,81/64,4/3,3/2,27/16,243/128]

-- * <http://anaphoria.com/novavotreediamond.pdf> (Novaro)

ew_novarotreediamond_1 :: ([[Rational]],[[Rational]])
ew_novarotreediamond_1 =
  let rem_oct x = if last x /= 2 then error "rem_oct?" else T.drop_last x
      add_oct x = if last x >= 2 then error "add_oct?" else x ++ [2]
      r_to_i = T.d_dx_by (/) . add_oct
      i_to_r = rem_oct . scanl (*) 1
      r_0 = [1,5/4,4/3,3/2,5/3,7/4]
      i_0 = r_to_i r_0
      i = T.rotations i_0
  in (i,map i_to_r i)

{- | P.1 {SCALA=NIL}

23-tone 7-limit (2004)

> ew_scl_find_r ew_novarotreediamond_1_r
-}
ew_novarotreediamond_1_r :: [Rational]
ew_novarotreediamond_1_r = r_normalise (concat (snd ew_novarotreediamond_1))

ew_novarotreediamond_1_scl :: T.Scale
ew_novarotreediamond_1_scl = r_to_scale "ew_novarotreediamond_1" "EW, novavotreediamond.pdf, P.1" ew_novarotreediamond_1_r

-- * <http://anaphoria.com/Pelogflute.pdf>

{- | P.2 {SCALA=NIL}

9-tone Pelog cycle (1988)

> ew_scl_find_r ew_pelogFlute_2
-}
ew_Pelogflute_2_r :: Fractional n => [n]
ew_Pelogflute_2_r = [1,16/15,64/55,5/4,4/3,16/11,8/5,128/75,20/11]

ew_Pelogflute_2_scl :: T.Scale
ew_Pelogflute_2_scl = r_to_scale "ew_Pelogflute_2" "EW, Pelogflute.pdf, P.2" ew_Pelogflute_2_r


-- * <http://anaphoria.com/xen1.pdf>

-- | P.9, Fig. 3
xen1_fig3 :: (SBT_NODE,Int)
xen1_fig3 = ((NIL,(1,3),(2,5),(1,2)),5)

-- | P.9, Fig. 4
xen1_fig4 :: (SBT_NODE,Int)
xen1_fig4 = ((NIL,(2,5),(5,12),(3,7)),5)

-- * <http://anaphoria.com/xen3b.pdf>

-- | P.3 Turkisk Baglama Scale {11-limit, SCALA=NIL}
ew_xen3b_3_gen :: [(Rational,Int)]
ew_xen3b_3_gen = [(1/(3^.6),12),(1/11,2),(5/3,3)]

ew_xen3b_3_r :: [Rational]
ew_xen3b_3_r = m3_gen_to_r ew_xen3b_3_gen

ew_xen3b_3_scl :: T.Scale
ew_xen3b_3_scl = r_to_scale "ew_xen3b_3" "EW, xen3b.pdf, P.3" ew_xen3b_3_r

-- > map length xen3b_9_i == [5,7,12,19,31]
xen3b_9_i :: [[Rational]]
xen3b_9_i =
  [[6/5,                                             10/9,                          9/8,                           6/5,                                             10/9]
  ,[16/15,9/8,                                       10/9,                          9/8,                           16/15,9/8,                                       10/9]
  ,[16/15,135/128,16/15,                             25/24,16/15,                   16/15,135/128,                 16/15,135/128,16/15,                             25/24,16/15]
  ,[28/27,36/35,135/128,28/27,36/35,                 25/24,28/27,36/35,             28/27,36/35,135/128,           28/27,36/35,135/128,28/27,36/35,                 25/24,28/27,36/35]
  ,[64/63,49/48,36/35,45/44,33/32,64/63,49/48,36/35, 45/44,55/54,64/63,49/48,36/35, 64/63,49/48,36/35,45/44,33/32, 64/63,49/48,36/35,45/44,33/32,64/63,49/48,36/35, 45/44,55/54,64/63,49/48,36/35]]

{- | P.9 {SCALA 5=nil 7=ptolemy_idiat 12=nil 19=wilson2 31=wilson_31}

> mapM ew_scl_find_r xen3b_9_r
-}
xen3b_9_r :: [[Rational]]
xen3b_9_r = map (T.drop_last . scanl (*) 1) xen3b_9_i

-- > map length xen3b_13_i == [5,7,12,17,22]
xen3b_13_i :: [[Rational]]
xen3b_13_i =
  [[7/6,                           8/7,                     9/8,                     7/6,                           8/7]
  ,[28/27,9/8,                     8/7,                     9/8,                     28/27,9/8,                     8/7]
  ,[28/27,243/224,28/27,           10/9,36/35,              28/27,243/224,           28/27,243/224,28/27,           10/9,36/35]
  ,[28/27,36/35,135/128,28/27,     36/35,175/162,36/35,     28/27,36/35,135/128,     28/27,36/35,135/128,28/27,     36/35,175/162,36/35]
  ,[28/27,36/35,25/24,81/80,28/27, 36/35,25/24,28/27,36/35, 28/27,36/35,25/24,81/80, 28/27,36/35,25/24,81/80,28/27, 36/35,25/24,28/27,36/35]]

-- | P.13 {SCALA 5=slendro5_2 7=ptolemy_diat2 12=nil 17=nil 22=wilson7_4}
xen3b_13_r :: [[Rational]]
xen3b_13_r = map (T.drop_last . scanl (*) 1) xen3b_13_i

-- * <http://anaphoria.com/xen3bappendix.pdf>

{- | PP.1-2 {SCALA: 22=wilson7_4}

17,31,41 lattices from XEN3B (1975)
-}
ew_xen3b_apx_gen :: [(Int,[M3_GEN])]
ew_xen3b_apx_gen =
  [(17,[(1/729,12)
       ,(5/3,3)
       ,(11,2)])
  ,(31,[(1/3,5)
       ,(5,2),(1/(5*(3^.2)),5)
       ,(7/(3^.4),5),(1/(7*(3^.4)),5)
       ,(1/11,5)
       ,((1/3)*(1/7)*5,2)
       ,((1/(7*(3^.3))) * 5,2)])
  ,(41,[(1/(3^.6),12)
       ,(5/(3^.3),5),(1/(5*(3^.2)),5)
       ,(7/(3^.4),7),(1/(7*(3^.3)),7)
       ,(11,5)])
  ,(22,[(1/3,5)
       ,(5/(3^.3),5),(1/(5*(3^.2)),5)
       ,(7/(3^.4),5)
       ,(7/(3^.3)*5,2)])]

ew_xen3b_apx_r :: [(Int,[Rational])]
ew_xen3b_apx_r =
  let f (k,g) = (k,r_normalise (concatMap m3_gen_unfold g))
  in map f ew_xen3b_apx_gen

-- * <http://anaphoria.com/xen456.pdf>

ew_xen456_7_gen :: [M3_GEN]
ew_xen456_7_gen = [(25/24,4),(5/3,4),(4/3,4),(16/15,4),(32/25,3)]

{- P.7 {SCALA=wilson1}

19-tone "A Scale for Scott" (1976)

> L.ew_find_scl_name ew_xen456_7_r -- wilson1
-}
ew_xen456_7_r :: [Rational]
ew_xen456_7_r = m3_gen_to_r ew_xen456_7_gen

ew_xen456_9_gen :: [M3_GEN]
ew_xen456_9_gen =
  [(1/(3^.3),4)
  ,(1/(5*(3^.2)),3)
  ,(1/(7*3),3)
  ,(1/11,3)
  ,(5/(11*3),4)
  ,(7/11,2)]

{- | P.9 {SCALA=NIL}

19-tone scale for the Clavichord-19 (1976)

> ew_scl_find_r ew_xen456_9_r

> import qualified Music.Theory.List as T {- hmt -}
> T.scl_find_ji T.is_subset ew_xen456_9_r -- NIL
-}
ew_xen456_9_r :: [Rational]
ew_xen456_9_r = m3_gen_to_r ew_xen456_9_gen

ew_xen456_9_scl :: T.Scale
ew_xen456_9_scl = r_to_scale "ew_xen456_9" "EW, xen456.pdf, P.9" ew_xen456_9_r

-- * GEMS

{- | <http://wilsonarchives.blogspot.com/2010/10/scale-for-rod-poole.html>

13-limit 22-tone scale {SCALA=nil}

> ew_scl_find_r ew_poole_r
-}
ew_poole_r :: [Rational]
ew_poole_r =
  [1,11*3,7*3/5,13/3,3*3,7/3,11/(3*3),5,7/11,1/3
  ,11,7/5,13/(3*3),3,7/(3*3),11/(3*3*3),5/3,3*3*3,7,11/3,5*3,7*3/11]

ew_poole_scl :: T.Scale
ew_poole_scl = r_to_scale "ew_poole" "EW, 2010/10/scale-for-rod-poole.html" ew_poole_r

{- | <http://wilsonarchives.blogspot.com/2014/05/an-11-limit-centaur-implied-in-wilson.html>

11-limit 17-tone scale {SCALA=wilcent17}

> ew_scl_find_r ew_centaur17_r
-}
ew_centaur17_r :: [Rational]
ew_centaur17_r = [1,11/(3*7),11/5,3*3,7/3,11/(3*3),5,1/3,11,11/(3*5),3,11/7,11/(3*3*3),5/3,7,11/3,3*5]

{- | <http://wilsonarchives.blogspot.com/2018/03/an-unusual-22-tone-7-limit-tuning.html>

7-limit 22-tone scale {SCALA=nil}

> ew_scl_find_r ew_two_22_7_r
-}
ew_two_22_7_r :: [Rational]
ew_two_22_7_r =
  [1/1,9/35,1/15,35/1,9/1,7/3,3/5,315/1,245/3,21/1,27/5
  ,7/5,735/1,189/1,49/1,63/5,5/3,3/7,1/9,1/35,15/1,35/9]

ew_two_22_7_scl :: T.Scale
ew_two_22_7_scl = r_to_scale "ew_two_22_7" "EW, 2018/03/an-unusual-22-tone-7-limit-tuning.html" ew_two_22_7_r

-- * DB

{- | Scales /not/ present in the standard scala file set.

> mapM_ (T.scale_wr_dir "/home/rohan/sw/hmt/data/scl/") ew_scl_db
> map T.scale_name ew_scl_db
-}
ew_scl_db :: [T.Scale]
ew_scl_db =
  [ew_1357_3_scl
  ,ew_el12_7_scl
  ,ew_el12_12_scl
  ,ew_hel_12_scl
  ,ew_novarotreediamond_1_scl
  ,ew_Pelogflute_2_scl
  ,ew_xen3b_3_scl
  ,ew_xen456_9_scl
  ,ew_poole_scl
  ,ew_two_22_7_scl
  ]

-- Local Variables:
-- truncate-lines:t
-- End:
