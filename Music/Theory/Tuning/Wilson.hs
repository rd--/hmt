-- | Erv Wilson, archives <http://anaphoria.com/wilson.html>
module Music.Theory.Tuning.Wilson where

import Control.Monad {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Ord {- base -}
import Data.Ratio {- base -}
import Text.Printf {- base -}

import qualified Safe {- safe -}

import qualified Music.Theory.Array.Text as Text {- hmt-base -}
import qualified Music.Theory.Function as Function {- hmt-base -}
import qualified Music.Theory.Graph.Type as Graph {- hmt-base -}
import qualified Music.Theory.List as List {- hmt-base -}
import qualified Music.Theory.Math as Math {- hmt-base -}
import qualified Music.Theory.Math.Convert as Convert {- hmt-base -}
import qualified Music.Theory.Show as Show {- hmt-base -}
import qualified Music.Theory.Tuple as Tuple {- hmt-base -}

import qualified Music.Theory.Graph.Dot as Dot {- hmt -}
import qualified Music.Theory.Interval.Barlow_1987 as Barlow {- hmt -}
import qualified Music.Theory.Math.Oeis as OEIS {- hmt -}
import qualified Music.Theory.Math.Prime as Prime {- hmt -}
import qualified Music.Theory.Set.List as Set {- hmt -}
import qualified Music.Theory.Tuning as Tuning {- hmt -}
import qualified Music.Theory.Tuning.Scala as Scala {- hmt -}

-- * Geom (see "Data.CG.Minus.Plain")

type V2 n = (n,n)
v2_map :: (t -> u) -> V2 t -> V2 u
v2_map f (a,b) = (f a,f b)
v2_zip :: (a -> b -> c) -> V2 a -> V2 b -> V2 c
v2_zip f (i,j) (p,q) = (f i p,f j q)
v2_add :: Num n => V2 n -> V2 n -> V2 n
v2_add = v2_zip (+)
v2_sum :: Num n => [V2 n] -> V2 n
v2_sum = foldl v2_add (0,0)
v2_scale :: Num n => n -> V2 n -> V2 n
v2_scale n = v2_map (* n)

-- * Pt Set

{- | Normalise set of points to lie in (-1,-1) - (1,1), scaling symetrically about (0,0)

> pt_set_normalise_sym [(40,0),(0,40),(13,11),(-8,4)] == [(1,0),(0,1),(0.325,0.275),(-0.2,0.1)]
> pt_set_normalise_sym [(-10,0),(1,10)] == [(-1,0),(0.1,1)]
-}
pt_set_normalise_sym :: (Fractional n,Ord n) => [V2 n] -> [V2 n]
pt_set_normalise_sym x =
  let z = maximum (map (uncurry max . Function.bimap1 abs) x)
  in map (v2_scale (recip z)) x

-- * Lattice Design

-- | /k/-unit co-ordinates for /k/-lattice.
type Lattice_Design n = (Int,[V2 n])

-- | Erv Wilson standard lattice, unit co-ordinates for 5-dimensions, ie. [3,5,7,11,13]
--
-- <http://anaphoria.com/wilsontreasure.html>
ew_lc_std :: Num n => Lattice_Design n
ew_lc_std = (5,[(20,0),(0,20),(4,3),(-3,4),(-1,2)])

-- | Kraig Grady standard lattice, unit co-ordinates for 5-dimensions, ie. [3,5,7,11,13]
--
-- <http://anaphoria.com/wilsontreasure.html>
kg_lc_std :: Num n => Lattice_Design n
kg_lc_std = (5,[(40,0),(0,40),(13,11),(-14,18),(-8,4)])

-- | Erv Wilson tetradic lattice (3-lattice), used especially when working with hexanies or 7 limit tunings
--
-- <http://anaphoria.com/wilsontreasure.html>
ew_lc_tetradic :: Num n => Lattice_Design n
ew_lc_tetradic = (3,[(-4,-2),(6,1),(5,-2)])

-- * Lattice_Factors

-- | A discrete /k/-lattice is described by a sequence of /k/-factors.
--   Values are ordinarily though not necessarily primes beginning at three.
type Lattice_Factors i = (Int,[i])

-- | Positions in a /k/-lattice are given as a /k/-list of steps.
type Lattice_Position = (Int,[Int])

-- | Delete entry at index.
lc_pos_del :: Int -> Lattice_Position -> Lattice_Position
lc_pos_del ix (k,x) = (k - 1,List.remove_ix ix x)

-- | Resolve Lattice_Position against Lattice_Design to V2
lc_pos_to_pt :: (Fractional n, Ord n) => Lattice_Design n -> Lattice_Position -> V2 n
lc_pos_to_pt (_,lc) (_,x) = v2_sum (zipWith (v2_scale . fromIntegral) x (pt_set_normalise_sym lc))

-- | White-space pretty printer for Lattice_Position.
--
-- > pos_pp_ws (3,[0,-2,1]) == "  0 -2  1"
pos_pp_ws :: Lattice_Position -> String
pos_pp_ws = let f x = printf "%3d" x in concatMap f . snd

-- | Given Lattice_Factors [X,Y,Z..] and Lattice_Position [x,y,z..], calculate the indicated ratio.
--
-- > lat_res (2,[3,5]) (2,[-5,2]) == (5 * 5) / (3 * 3 * 3 * 3 * 3)
lat_res :: Integral i => Lattice_Factors i -> Lattice_Position -> Ratio i
lat_res (_,p) (_,q) =
  let f i j = case compare j 0 of
                GT -> (i ^ Convert.int_to_integer j) % 1
                EQ -> 1
                LT -> 1 % (i ^ abs (Convert.int_to_integer j))
  in product (zipWith f p q)

-- * Rat (n,d)

-- | Ratio given as (/n/,/d/)
type Rat = (Integer,Integer)

-- | Remove all octaves from /n/ and /d/.
rat_rem_oct :: Rat -> Rat
rat_rem_oct = Function.bimap1 (product . filter (/= 2)) . Prime.rat_prime_factors

-- | Lift 'Rat' function to 'Rational'.
rat_lift_1 :: (Rat -> Rat) -> Rational -> Rational
rat_lift_1 f = uncurry (%) . f . Math.rational_nd

-- | Convert 'Rat' to 'Rational'
rat_to_ratio :: Rat -> Rational
rat_to_ratio (n,d) = n % d

-- | Mediant, ie. n1+n2/d1+d2
--
-- > rat_mediant (0,1) (1,2) == (1,3)
rat_mediant :: Rat -> Rat -> Rat
rat_mediant (n1,d1) (n2,d2) = (n1 + n2,d1 + d2)

-- | Rat written as n/d
rat_pp :: Rat -> String
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
r_seq_limit = maximum . map Prime.rational_prime_limit

-- | Find factors of set of ratios, ie. the union of all factor in both numerator & denominator.
--
-- > r_seq_factors [1/3,5/7,9/8,13,27,31] == [2,3,5,7,13,31]
r_seq_factors :: [Rational] -> [Integer]
r_seq_factors = nub . sort . concatMap (uncurry (++) . Prime.rational_prime_factors)

r_seq_normalize :: [Rational] -> [Rational]
r_seq_normalize r =
  let r0:_ = r
      r' = map (Tuning.fold_ratio_to_octave_err . (/ r0)) r
  in sort r'

r_seq_rotations :: [Rational] -> [[Rational]]
r_seq_rotations = map r_seq_normalize . List.rotations

-- * Table

-- | Vector of prime-factors up to /limit/.
--
-- > map (rat_fact_lm 11) [3,5,7,2/11] == [(5,[0,1,0,0,0]),(5,[0,0,1,0,0]),(5,[0,0,0,1,0]),(5,[1,0,0,0,-1])]
rat_fact_lm :: Integer -> Rational -> Lattice_Position
rat_fact_lm lm =
  let k = fromMaybe 1 (Prime.prime_k lm) + 1
  in (\c -> (k,c)) .
     Prime.rat_prime_factors_t k .
     Math.rational_nd

tbl_txt :: Bool -> Integer -> [Rational] -> [[String]]
tbl_txt del lm_z rs =
  let lm = r_seq_limit rs
      scl = map ((if del then lc_pos_del 0 else id) . rat_fact_lm lm) rs
      cs = map (Tuning.ratio_to_cents . Tuning.fold_ratio_to_octave_err) rs
      hs = map (Barlow.harmonicity_r Barlow.barlow) rs :: [Double]
      f (k,x,r,c,h) = [show k
                      ,if lm <= lm_z then pos_pp_ws x else "..."
                      ,Show.ratio_pp r
                      ,Show.real_pp 2 c
                      ,Show.real_pp_unicode 2 h]
  in map (intersperse "=" . f) (zip5 [0::Int ..] scl rs cs hs)

-- > tbl_wr False [1,7/6,5/4,4/3,3/2]
-- > tbl_wr True [1,3,1/5,15/31]
tbl_wr :: Bool -> [Rational] -> IO ()
tbl_wr del = putStr . unlines . Text.table_pp (False,True,False," ",False) . tbl_txt del 31

-- * Graph

-- | (maybe (maybe lattice-design, maybe primes),gr-attr,vertex-pp)
type Ew_Gr_Opt = (Maybe (Lattice_Design Rational,Maybe [Integer]),[Dot.Dot_Meta_Attr],Rational -> String)

ew_gr_opt_pos :: Ew_Gr_Opt -> Bool
ew_gr_opt_pos (lc_m,_,_) = isJust lc_m

-- > map (ew_gr_r_pos ew_lc_std (Just [3,5,31])) [3,5,31]
ew_gr_r_pos :: Lattice_Design Rational -> Maybe [Integer] -> Rational -> Dot.Dot_Attr
ew_gr_r_pos (k,lc) primes_l =
  let f m (x,y) = (m * x,m * y)
  in Dot.node_pos_attr .
     f 160 .
     lc_pos_to_pt (k,lc) .
     (\c -> (k,c)) .
     -- this is a little subtle, tail removes the '2' slot from rational_prime_factors_t
     maybe (tail . Prime.rational_prime_factors_t (k + 1)) Prime.rational_prime_factors_c primes_l

-- | 'Dot.lbl_to_udot' add position attribute if a 'Lattice_Design' is given.
ew_gr_udot :: Ew_Gr_Opt -> Graph.Lbl Rational () -> [String]
ew_gr_udot (lc_m,attr,v_pp) =
  let (e,p_f) = case lc_m of
                  Nothing -> ("sfdp",const Nothing)
                  Just (lc,primes_l) -> ("neato",Just . ew_gr_r_pos lc primes_l)
  in Dot.lbl_to_udot
     ([("graph:layout",e),("node:shape","plain")] ++ attr) -- ("graph:K","0.6") ("edge:len","1.0")
     (\(_,v) -> List.mcons (p_f v) [("label",v_pp v)]
     ,const [])

-- | 'writeFile' of 'ew_gr_udot'
ew_gr_udot_wr :: Ew_Gr_Opt -> FilePath -> Graph.Lbl Rational () -> IO ()
ew_gr_udot_wr opt fn = writeFile fn . unlines . ew_gr_udot opt

ew_gr_udot_wr_svg :: Ew_Gr_Opt -> FilePath -> Graph.Lbl Rational () -> IO ()
ew_gr_udot_wr_svg opt fn gr = do
  ew_gr_udot_wr opt fn gr
  void (Dot.dot_to_svg (if ew_gr_opt_pos opt then ["-n"] else []) fn)

-- * Zig-Zag

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

-- * Mos

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
  in if Tuple.t2_sum y == 3 then [x,y] else x : mos_unfold y

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
      ((i0,j0), r) = List.headTail (mos p g)
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

-- * Mos/Log

mos_recip_seq :: Double -> [(Int,Double)]
mos_recip_seq x = let y = truncate x in (y,x) : mos_recip_seq (recip (x - fromIntegral y))

-- > take 3 (mos_log (5/4)) == [(3,3.10628371950539),(9,9.408778735385603),(2,2.4463112031908785)]
mos_log :: Double -> [(Int,Double)]
mos_log r = mos_recip_seq (recip (logBase 2 r))

-- > take 9 (mos_log_kseq 1.465571232) == [1,1,4,2,1,3,1,6,2]
mos_log_kseq :: Double -> [Int]
mos_log_kseq = map fst . mos_log

-- * Stern-Brocot Tree

data Sbt_Div = Nil | Lhs | Rhs deriving (Show)
type Sbt_Node = (Sbt_Div,Rat,Rat,Rat)

sbt_step :: Sbt_Node -> [Sbt_Node]
sbt_step (_,l,m,r) = [(Lhs,l,rat_mediant l m, m),(Rhs,m,rat_mediant m r,r)]

-- sbt = stern-brocot tree
sbt_root :: Sbt_Node
sbt_root = (Nil,(0,1),(1,1),(1,0))

sbt_half :: Sbt_Node
sbt_half = (Nil,(0,1),(1,2),(1,1))

-- > sbt_from sbt_root
sbt_from :: Sbt_Node -> [[Sbt_Node]]
sbt_from = iterate (concatMap sbt_step) . return

sbt_k_from :: Int -> Sbt_Node -> [[Sbt_Node]]
sbt_k_from k = take k . sbt_from

sbt_node_to_edge :: Sbt_Node -> String
sbt_node_to_edge (dv,l,m,r) =
  let edge_pp p q = printf "\"%s\" -- \"%s\"" (rat_pp p) (rat_pp q)
  in case dv of
       Nil -> ""
       Lhs -> edge_pp r m
       Rhs -> edge_pp l m

sbt_node_elem :: Sbt_Node -> [Rat]
sbt_node_elem (dv,l,m,r) =
  case dv of
    Nil -> [l,m,r]
    _ -> [m]

sbt_dot :: [Sbt_Node] -> [String]
sbt_dot n =
  let e = map sbt_node_to_edge n
  in concat [["graph {","node [shape=plain]"],e,["}"]]

-- * M-Gen

(^.) :: Rational -> Int -> Rational
(^.) = (^)

r_normalise :: [Rational] -> [Rational]
r_normalise = nub . sortOn Tuning.fold_ratio_to_octave_err

-- | (ratio,multiplier,steps)
type M_Gen = (Rational,Rational,Int)

m_gen_unfold :: M_Gen -> [Rational]
m_gen_unfold (r,m,n) = take n (iterate (* m) r)

m_gen_to_r :: [M_Gen] -> [Rational]
m_gen_to_r = r_normalise . concatMap m_gen_unfold

-- * M3-Gen

-- | (ratio,M3-steps)
type M3_Gen = (Rational,Int)

m3_to_m :: M3_Gen -> M_Gen
m3_to_m (r,n) = (r,3,n)

-- > map m3_gen_unfold [(3,4),(21/9,4),(15/9,4),(35/9,3),(21/5,4),(27/5,3)]
m3_gen_unfold :: M3_Gen -> [Rational]
m3_gen_unfold = m_gen_unfold . m3_to_m

m3_gen_to_r :: [M3_Gen] -> [Rational]
m3_gen_to_r = r_normalise . concatMap m3_gen_unfold

-- * Scala

r_to_scale :: String -> String -> [Rational] -> Scala.Scale
r_to_scale nm dsc r =
  let r' = map Tuning.fold_ratio_to_octave_err (tail r) ++ [2]
  in if r !! 0 /= 1 || not (List.is_ascending r')
     then error "r_to_scale?"
     else (nm,dsc,length r,map Right r')

ew_scl_find_r_eq :: [Rational] -> [Scala.Scale] -> [String]
ew_scl_find_r_eq r =
  let r' = map Tuning.fold_ratio_to_octave_err r
  in if head r' /= 1
     then error "ew_scl_find_r_eq?: r'0 /= 1"
     else map Scala.scale_name . Scala.scl_find_ji (==) (r' ++ [2])

ew_scl_find_r_rot :: [Rational] -> [Scala.Scale] -> [String]
ew_scl_find_r_rot r db = concatMap (\s -> ew_scl_find_r_eq s db) (r_seq_rotations r)

-- * <http://anaphoria.com/1-3-5-7-9Genus.pdf>

ew_1357_3_gen :: [M3_Gen]
ew_1357_3_gen = [(3,4),(21/9,4),(15/9,4),(35/9,3),(21/5,4),(27/5,3)]

{- | P.3 7-limit {Scala=nil}

> db <- Scala.scl_load_db
> ew_scl_find_r_rot (1 : ew_1357_3_r) db == []
-}
ew_1357_3_r :: [Rational]
ew_1357_3_r = r_normalise (concatMap m3_gen_unfold ew_1357_3_gen)

ew_1357_3_scl :: Scala.Scale
ew_1357_3_scl = r_to_scale "ew_1357_3" "EW, 1-3-5-7-9Genus.pdf, P.3" (1 : ew_1357_3_r)

-- * <http://anaphoria.com/earlylattices12.pdf>

{- | P.7 11-limit {Scala=nil}

> ew_scl_find_r_rot ew_el12_7_r db == []
-}
ew_el12_7_r :: [Rational]
ew_el12_7_r = [1,5/(7*11),1/7,7*11,7*11*11/5,11,5/7,1/11,7*11*11,1/(7*11),11*11,7*11/5]

ew_el12_7_scl :: Scala.Scale
ew_el12_7_scl = r_to_scale "ew_el12_7" "EW, earlylattices12.pdf, P.7" ew_el12_7_r

{- | P.9 7-limit {Scala=wilson_class}

> ew_scl_find_r_eq ew_el12_9_r db == ["wilson_class"]
-}
ew_el12_9_r :: [Rational]
ew_el12_9_r = [1,5*5/3,7/(5*5),7/3,5,1/3,7/5,5*7/3,1/5,5/3,7,7/(3*5)]

--ew_el12_9_scl :: Scala.Scale
--ew_el12_9_scl = r_to_scale "ew_el12_9" "EW, earlylattices12.pdf, P.9" ew_el12_9_r

{- | P.12 11-limit {Scala=nil}

> ew_scl_find_r_rot ew_el12_12_r db == []
-}
ew_el12_12_r :: [Rational]
ew_el12_12_r = [1,3*3*5/11,3/11,7/3,5,7/11,3*5/11,5*7/3,7/(3*3),5*7/11,7/(3*11),3*5]

ew_el12_12_scl :: Scala.Scale
ew_el12_12_scl = r_to_scale "ew_el12_12" "EW, earlylattices12.pdf, P.12" ew_el12_12_r

-- * <http://anaphoria.com/earlylattices22.pdf>

{- | P.2 11-limit {Scala=wilson_l4}

> ew_scl_find_r_eq ew_el22_2_r db == ["wilson_l4"]
-}
ew_el22_2_r :: [Rational]
ew_el22_2_r =
  [1,7*7/3,3*7/5,5/(3*3),1/7,7/3,3/5,5,5*7/(3*3*3),1/3,7*7/(3*3)
  ,7/5,5*7/3,3,7/(3*3),1/5,5/3,3/7,7,3*3/5,7/(3*5),5*7/(3*3)]

{- | P.3 11-limit {Scala=wilson_l5}

> ew_scl_find_r_eq ew_el22_3_r db == ["wilson_l5"]
-}
ew_el22_3_r :: [Rational]
ew_el22_3_r =
  [1,7*7/3,7*11/(3*3),3/11,1/7,7/3,3/5,5,7/11,1/3,7*7/(3*3)
  ,7/5,5*7/3,3,7/(3*3),1/5,5/3,3/7,7,11/3,7/(3*5),5*7/(3*3)]

{- | P.4 11-limit {Scala=wilson_l3}

> ew_scl_find_r_eq ew_el22_4_r db == ["wilson_l3"]
-}
ew_el22_4_r :: [Rational]
ew_el22_4_r =
  [1,3*11,3*7/5,5*7,3*3,7/3,3/5,5,7/11,3*7,11
  ,7/5,5*7/3,3,7/(3*3),1/5,3*5*7,3*3*3,7,3*3/5,3*5,3*7/11]

{- | P.5 11-limit {Scala=wilson_l1}

> ew_scl_find_r_eq ew_el22_5_r db == ["wilson_l1"]
-}
ew_el22_5_r :: [Rational]
ew_el22_5_r =
  [1,3*11,3*7/5,5*7,3*3,7/3,7*11,5,3*5*11,3*7,11
  ,7/5,3*7*11/5,3,3*3*11,7*11/3,3*11/5,5*11,7,3*7*11,3*5,7*11/5]

{- | P.6 11-limit {Scala=wilson_l2}

> ew_scl_find_r_eq ew_el22_6_r db == ["wilson_l2"]
-}
ew_el22_6_r :: [Rational]
ew_el22_6_r =
  [1,7*7/3,7*11/(3*3),11/5,3*3,7/3,7*11,5,7*11/(3*5),1/3,11
  ,7*11/(3*3*3),5*7/3,3,11/7,7*11/3,5/3,7*11/(3*3*5),7,11/3,3*5,7*11/5]

-- * <http://anaphoria.com/diamond.pdf>

ew_diamond_mk :: [Integer] -> [Rational]
ew_diamond_mk u = r_normalise [x % y | x <- u, y <- u]

-- > m3_gen_to_r ew_diamond_12_gen == ew_diamond_12_r
ew_diamond_12_gen :: [M3_Gen]
ew_diamond_12_gen =
  [(1/(3^.2),5),(5/(3^.2),3),(7/(3^.2),3),(11/(3^.2),3)
  ,(1/5,3),(1/7,3),(1/11,3)
  ,(5/7,1),(5/11,1),(7/5,1),(7/11,1),(11/5,1),(11/7,1)]

{- | P.7 & P.12 11-limit {Scala=partch_29}

1,3,5,7,9,11 diamond

> ew_scl_find_r_eq ew_diamond_12_r db == ["partch_29"]
-}
ew_diamond_12_r :: [Rational]
ew_diamond_12_r = ew_diamond_mk [1,3,5,7,9,11]

{- | P.10 & P.13 13-limit {Scala=novaro15}

1,3,5,7,9,11,13,15 diamond

> ew_scl_find_r_eq ew_diamond_13_r db == ["novaro15"]
-}
ew_diamond_13_r :: [Rational]
ew_diamond_13_r = ew_diamond_mk [1,3,5,7,9,11,13,15]

-- * <http://anaphoria.com/hel.pdf>

hel_r_asc :: (Integer,Integer) -> [Rational]
hel_r_asc (n,d) = n%d : hel_r_asc (n+1,d+1)

type Hel = ([Rational],[Rational])

-- | P.6
hel_1_i :: Hel
hel_1_i =
  let i = take 6 (hel_r_asc (7,6))
  in (take 5 i,take 5 (List.rotate_left 2 i))

-- | P.6
hel_2_i :: Hel
hel_2_i =
  let i = take 10 (hel_r_asc (9,8))
  in (take 8 (List.rotate_left 3 (tail i))
     ,take 7 i)

-- | P.10
hel_3_i :: Hel
hel_3_i =
  let i = take 16 (hel_r_asc (15,14))
  in (take 13 (List.rotate_left 6 (take 14 i)),take 14 (tail i))

hel_r :: Hel -> [[Rational]]
hel_r (p,q) =
  let i_to_r = scanl (*) 1
  in [i_to_r p,i_to_r q,r_normalise (concat [i_to_r p,i_to_r q])]

{- | P.12 {Scala=nil}

22-tone 23-limit Evangalina tuning (2001)

> ew_scl_find_r_rot ew_hel_12_r db == []
-}
ew_hel_12_r :: [Rational]
ew_hel_12_r =
  [1,3*3*3*5,13/3,5/(3*3),3*3,7/3,11/(3*3),5,3*3*3*3,1/3,11
  ,3*3*5,17/3,3,3*3*3*3*5,13,5/3,3*3*3,7,11/3,3*5,23/3]

ew_hel_12_scl :: Scala.Scale
ew_hel_12_scl = r_to_scale "ew_hel_12" "EW, hel.pdf, P.12" ew_hel_12_r

-- * <http://anaphoria.com/HexanyStellatesExpansions.pdf>

-- > she_div "ABCD" == [["BCD","A"],["ACD","B"],["ABD","C"],["ABC","D"]]
she_div :: Eq a => [a] -> [[[a]]]
she_div x =
  let f = (== [1,length x - 1]) . sort . map length
  in map (sortOn (Down . length)) (filter f (Set.partitions x))

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
she_mul_r r = [x * y | x <- r,y <- r,x <= y]

{- | she = Stellate Hexany Expansions, P.10 {Scala=stelhex1,stelhex2,stelhex5,stelhex6, Scala.Rot=dkring3}

> she [1,3,5,7] == [1,21/20,15/14,35/32,9/8,5/4,21/16,35/24,3/2,49/32,25/16,105/64,7/4,15/8]
> map (flip ew_scl_find_r_eq db . she) [[1,3,5,7],[1,3,5,9],[1,3,7,9],[1,3,5,11]] == [["stelhex1"],["stelhex2"],["stelhex5"],["stelhex6"]]
> ew_scl_find_r_rot (she [1,3,7,9]) db == ["stelhex5","dkring3"]
> ew_scl_find_r_rot (she [1,(5*7)/(3*3),1/(3 * 5),1/3]) db == []
-}
she :: [Rational] -> [Rational]
she r = nub (sort (map Tuning.fold_ratio_to_octave_err (she_mul_r r ++ she_div_r r)))

-- * <http://anaphoria.com/meru.pdf>

-- > map (every_nth "abcdef") [1..3] == ["abcdef","ace","ad"]
every_nth :: [t] -> Int -> [t]
every_nth l k =
  case l of
    [] -> []
    x:_ -> x : every_nth (drop k l) k

meru :: Num n => [[n]]
meru =
  let f xs = zipWith (+) (0 : xs) (xs ++ [0])
  in iterate f [1]

-- > meru_k 13
meru_k :: Num n => Int -> [[n]]
meru_k k = take k meru

-- > map (sum . meru_1) [1 .. 13] == [1,1,2,3,5,8,13,21,34,55,89,144,233]
meru_1 :: Num n => Int -> [n]
meru_1 k = zipWith (flip (Safe.atDef 0)) [0..] (reverse (meru_k k))

-- > take 13 meru_1_direct == [1,1,2,3,5,8,13,21,34,55,89,144,233]
meru_1_direct :: Num n => [n]
meru_1_direct = tail OEIS.a000045

-- | Meru 2 = META-PELOG
--
-- > map (sum . meru_2) [1 .. 14] == [1,1,1,2,3,4,6,9,13,19,28,41,60,88]
meru_2 :: Num n => Int -> [n]
meru_2 k = zipWith (flip (Safe.atDef 0)) [0..] (every_nth (reverse (meru_k k)) 2)

-- > take 14 meru_2_direct == [1,1,1,2,3,4,6,9,13,19,28,41,60,88]
meru_2_direct :: Num n => [n]
meru_2_direct = OEIS.a000930

-- | meru_3 = META-SLENDRO
meru_3 :: Num n => Int -> [[n]]
meru_3 k =
  let f t = zipWith (flip (Safe.atDef 0)) [0,2..] t
      t0 = reverse (meru_k k)
      t1 = map tail t0
  in [f t0,f t1]

-- > map sum (meru_3_seq 13) == [1,0,1,1,1,2,2,3,4,5,7,9,12,16,21,28,37,49,65,86,114,151,200,265,351,465]
meru_3_seq :: Num n => Int -> [[n]]
meru_3_seq k = concatMap meru_3 [1 .. k]

-- > take 26 meru_3_direct == [1,0,1,1,1,2,2,3,4,5,7,9,12,16,21,28,37,49,65,86,114,151,200,265,351,465]
meru_3_direct :: Num n => [n]
meru_3_direct = drop 3 OEIS.a000931

-- > map (sum . meru_4) [1 .. 13] == [1,1,1,1,2,3,4,5,7,10,14,19,26]
meru_4 :: Num n => Int -> [n]
meru_4 k = zipWith (flip (Safe.atDef 0)) [0..] (every_nth (reverse (meru_k k)) 3)

-- > take 31 meru_4_direct == map (sum . meru_4) [1 .. 31]
meru_4_direct :: Num n => [n]
meru_4_direct = tail OEIS.a003269

-- > map meru_5 [1..4]
meru_5 :: Num n => Int -> [[n]]
meru_5 k =
  let f t = zipWith (flip (Safe.atDef 0)) [0,3..] t
      t0 = reverse (meru_k k)
  in map (\n -> f (map (drop n) t0)) [0 .. 2]

-- > map sum (meru_5_seq 13)
meru_5_seq :: Num n => Int -> [[n]]
meru_5_seq k = concatMap meru_5 [1 .. k]

-- > take 39 meru_5_direct == map sum (meru_5_seq 13)
meru_5_direct :: Num n => [n]
meru_5_direct = OEIS.a017817

-- > map (sum . meru_6) [1 .. 21] == [1,1,1,1,1,2,3,4,5,6,8,11,15,20,26,34,45,60,80,106,140]
meru_6 :: Num n => Int -> [n]
meru_6 k = zipWith (flip (Safe.atDef 0)) [0..] (every_nth (reverse (meru_k k)) 4)

-- > take 21 meru_6_direct == map (sum . meru_6) [1 .. 21]
meru_6_direct :: Num n => [n]
meru_6_direct = OEIS.a003520

-- > take 26 meru_7_direct == [0,1,0,1,0,1,1,1,2,1,3,2,4,4,5,7,7,11,11,16,18,23,29,34,45,52]
meru_7_direct :: Num n => [n]
meru_7_direct = OEIS.a001687

-- * <http://anaphoria.com/mos.pdf>

{- | P.13, tanabe {Scala=chin_7} {Scala.Rot=eratos_diat,syntonolydian,aeolic}

> ew_scl_find_r_eq ew_mos_13_tanabe_r db == ["chin_7"]
> ew_scl_find_r_rot ew_mos_13_tanabe_r db == ["chin_7","eratos_diat","syntonolydian","aeolic"]
-}
ew_mos_13_tanabe_r :: [Rational]
ew_mos_13_tanabe_r = [1,9/8,81/64,4/3,3/2,27/16,243/128]

-- * <http://anaphoria.com/novavotreediamond.pdf> (Novaro)

ew_novarotreediamond_1 :: ([[Rational]],[[Rational]])
ew_novarotreediamond_1 =
  let rem_oct x = if last x /= 2 then error "rem_oct?" else List.drop_last x
      add_oct x = if last x >= 2 then error "add_oct?" else x ++ [2]
      r_to_i = List.d_dx_by (/) . add_oct
      i_to_r = rem_oct . scanl (*) 1
      r_0 = [1,5/4,4/3,3/2,5/3,7/4]
      i_0 = r_to_i r_0
      i = List.rotations i_0
  in (i,map i_to_r i)

{- | P.1 {Scala=nil}

23-tone 7-limit (2004)

> ew_scl_find_r_rot ew_novarotreediamond_1_r db == []
-}
ew_novarotreediamond_1_r :: [Rational]
ew_novarotreediamond_1_r = r_normalise (concat (snd ew_novarotreediamond_1))

ew_novarotreediamond_1_scl :: Scala.Scale
ew_novarotreediamond_1_scl = r_to_scale "ew_novarotreediamond_1" "EW, novavotreediamond.pdf, P.1" ew_novarotreediamond_1_r

-- * <http://anaphoria.com/Pelogflute.pdf>

{- | P.2 {Scala=wilson_11-limit-pelog9}

9-tone Pelog cycle (1988)

> ew_scl_find_r_eq ew_Pelogflute_2_r db == ["wilson_11-limit-pelog9"]
-}
ew_Pelogflute_2_r :: Fractional n => [n]
ew_Pelogflute_2_r = [1,16/15,64/55,5/4,4/3,16/11,8/5,128/75,20/11]

-- ew_Pelogflute_2_scl :: Scala.Scale
-- ew_Pelogflute_2_scl = r_to_scale "ew_Pelogflute_2" "EW, Pelogflute.pdf, P.2" ew_Pelogflute_2_r

-- * <http://anaphoria.com/xen1.pdf>

-- | P.9, Fig. 3
xen1_fig3 :: (Sbt_Node,Int)
xen1_fig3 = ((Nil,(1,3),(2,5),(1,2)),5)

-- | P.9, Fig. 4
xen1_fig4 :: (Sbt_Node,Int)
xen1_fig4 = ((Nil,(2,5),(5,12),(3,7)),5)

-- * <http://anaphoria.com/xen3b.pdf>

{- | P.3 Turkisk Baglama Scale {11-limit, Scala=nil}

> ew_scl_find_r_rot ew_xen3b_3_r db == []
-}
ew_xen3b_3_gen :: [(Rational,Int)]
ew_xen3b_3_gen = [(1/(3^.6),12),(1/11,2),(5/3,3)]

ew_xen3b_3_r :: [Rational]
ew_xen3b_3_r = m3_gen_to_r ew_xen3b_3_gen

ew_xen3b_3_scl :: Scala.Scale
ew_xen3b_3_scl = r_to_scale "ew_xen3b_3" "EW, xen3b.pdf, P.3" ew_xen3b_3_r

-- > map length xen3b_9_i == [5,7,12,19,31]
xen3b_9_i :: [[Rational]]
xen3b_9_i =
  [[6/5,                                             10/9,                          9/8,                           6/5,                                             10/9]
  ,[16/15,9/8,                                       10/9,                          9/8,                           16/15,9/8,                                       10/9]
  ,[16/15,135/128,16/15,                             25/24,16/15,                   16/15,135/128,                 16/15,135/128,16/15,                             25/24,16/15]
  ,[28/27,36/35,135/128,28/27,36/35,                 25/24,28/27,36/35,             28/27,36/35,135/128,           28/27,36/35,135/128,28/27,36/35,                 25/24,28/27,36/35]
  ,[64/63,49/48,36/35,45/44,33/32,64/63,49/48,36/35, 45/44,55/54,64/63,49/48,36/35, 64/63,49/48,36/35,45/44,33/32, 64/63,49/48,36/35,45/44,33/32,64/63,49/48,36/35, 45/44,55/54,64/63,49/48,36/35]]

{- | P.9 {Scala: 5=nil 7=ptolemy_idiat 12=nil 19=wilson2 31=wilson_31} {Scala.Rot: 5=malkauns, 7=indian-magrama,ptolemy 12=malcolme}

> map (flip ew_scl_find_r_eq db) xen3b_9_r == [[],["ptolemy_idiat"],[],["wilson2"],["wilson_31"]]
> mapM ew_scl_find_r_rot xen3b_9_r db == [["malkauns"],["ptolemy_idiat","indian-magrama","ptolemy"],["malcolme"],["wilson2"],["wilson_31"]]
-}
xen3b_9_r :: [[Rational]]
xen3b_9_r = map (List.drop_last . scanl (*) 1) xen3b_9_i

-- > map length xen3b_13_i == [5,7,12,17,22]
xen3b_13_i :: [[Rational]]
xen3b_13_i =
  [[7/6,                           8/7,                     9/8,                     7/6,                           8/7]
  ,[28/27,9/8,                     8/7,                     9/8,                     28/27,9/8,                     8/7]
  ,[28/27,243/224,28/27,           10/9,36/35,              28/27,243/224,           28/27,243/224,28/27,           10/9,36/35]
  ,[28/27,36/35,135/128,28/27,     36/35,175/162,36/35,     28/27,36/35,135/128,     28/27,36/35,135/128,28/27,     36/35,175/162,36/35]
  ,[28/27,36/35,25/24,81/80,28/27, 36/35,25/24,28/27,36/35, 28/27,36/35,25/24,81/80, 28/27,36/35,25/24,81/80,28/27, 36/35,25/24,28/27,36/35]]

{- | P.13 {Scala: 5=slendro5_2 7=ptolemy_diat2 12=nil 17=nil 22=wilson7_4}

> map (flip ew_scl_find_r_eq db) xen3b_13_r == [["slendro5_2"],["ptolemy_diat2"],[],[],["wilson7_4"]]
> mapM ew_scl_find_r_rot xen3b_13_r db == [["slendro5_2","slendro5_1","slendro_7_4"],["ptolemy_diat2","tritriad14"],[],[],["wilson7_4"]]
-}
xen3b_13_r :: [[Rational]]
xen3b_13_r = map (List.drop_last . scanl (*) 1) xen3b_13_i

-- * <http://anaphoria.com/xen3bappendix.pdf>

{- | PP.1-2 {Scala: 22=wilson7_4}

17,31,41 lattices from XEN3B (1975)
-}
ew_xen3b_apx_gen :: [(Int,[M3_Gen])]
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

ew_xen456_7_gen :: [M3_Gen]
ew_xen456_7_gen = [(25/24,4),(5/3,4),(4/3,4),(16/15,4),(32/25,3)]

{- P.7 {Scala=wilson1}

19-tone "A Scale for Scott" (1976)

> ew_scl_find_r_eq ew_xen456_7_r db == ["wilson1"]
-}
ew_xen456_7_r :: [Rational]
ew_xen456_7_r = m3_gen_to_r ew_xen456_7_gen

ew_xen456_9_gen :: [M3_Gen]
ew_xen456_9_gen =
  [(1/(3^.3),4)
  ,(1/(5*(3^.2)),3)
  ,(1/(7*3),3)
  ,(1/11,3)
  ,(5/(11*3),4)
  ,(7/11,2)]

{- | P.9 {Scala=nil} {Scala:Rot=wilson11}

19-tone scale for the Clavichord-19 (1976)

> ew_scl_find_r_rot ew_xen456_9_r db == ["wilson11"]
-}
ew_xen456_9_r :: [Rational]
ew_xen456_9_r = m3_gen_to_r ew_xen456_9_gen

ew_xen456_9_scl :: Scala.Scale
ew_xen456_9_scl = r_to_scale "ew_xen456_9" "EW, xen456.pdf, P.9" ew_xen456_9_r

-- * Gems

{- | <http://wilsonarchives.blogspot.com/2010/10/scale-for-rod-poole.html>

13-limit 22-tone scale {Scala=nil}

> ew_scl_find_r_rot ew_poole_r db == []
-}
ew_poole_r :: [Rational]
ew_poole_r =
  [1,11*3,7*3/5,13/3,3*3,7/3,11/(3*3),5,7/11,1/3
  ,11,7/5,13/(3*3),3,7/(3*3),11/(3*3*3),5/3,3*3*3,7,11/3,5*3,7*3/11]

ew_poole_scl :: Scala.Scale
ew_poole_scl = r_to_scale "ew_poole" "EW, 2010/10/scale-for-rod-poole.html" ew_poole_r

{- | <http://wilsonarchives.blogspot.com/2014/05/an-11-limit-centaur-implied-in-wilson.html>

11-limit 17-tone scale {Scala=wilcent17}

> ew_scl_find_r_eq ew_centaur17_r db == ["wilcent17"]
-}
ew_centaur17_r :: [Rational]
ew_centaur17_r = [1,11/(3*7),11/5,3*3,7/3,11/(3*3),5,1/3,11,11/(3*5),3,11/7,11/(3*3*3),5/3,7,11/3,3*5]

{- | <http://wilsonarchives.blogspot.com/2018/03/an-unusual-22-tone-7-limit-tuning.html>

7-limit 22-tone scale {Scala=nil}

> ew_scl_find_r_rot ew_two_22_7_r db == []
-}
ew_two_22_7_r :: [Rational]
ew_two_22_7_r =
  [1,9/35,1/15,35,9,7/3,3/5,315,245/3,21,27/5
  ,7/5,735,189,49,63/5,5/3,3/7,1/9,1/35,15,35/9]

ew_two_22_7_scl :: Scala.Scale
ew_two_22_7_scl = r_to_scale "ew_two_22_7" "EW, 2018/03/an-unusual-22-tone-7-limit-tuning.html" ew_two_22_7_r

-- * Db

{- | Scales /not/ present in the standard scala file set.

> mapM_ (Scala.scale_wr_dir "/home/rohan/sw/hmt/data/scl/") ew_scl_db
> map Scala.scale_name ew_scl_db
-}
ew_scl_db :: [Scala.Scale]
ew_scl_db =
  [ew_1357_3_scl
  ,ew_el12_7_scl
  ,ew_el12_12_scl
  ,ew_hel_12_scl
  ,ew_novarotreediamond_1_scl
  ,ew_xen3b_3_scl
  ,ew_xen456_9_scl
  ,ew_poole_scl
  ,ew_two_22_7_scl
  ]

-- Local Variables:
-- truncate-lines:t
-- End:
