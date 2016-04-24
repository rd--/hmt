module Music.Theory.Tuning.Euler where

import Data.List {- base -}
import Data.Ratio {- base -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Math as T {- hmt -}
import qualified Music.Theory.Pitch.Note as T {- hmt -}
import qualified Music.Theory.Tuning as T {- hmt -}
import qualified Music.Theory.Tuple as T {- hmt -}

-- | 'T.fold_ratio_to_octave' of '*'.
rat_mul :: Rational -> Rational -> Rational
rat_mul r = T.fold_ratio_to_octave . (* r)

-- | 'T.fold_ratio_to_octave' of '/'.
rat_div :: Rational -> Rational -> Rational
rat_div p q = T.fold_ratio_to_octave (p / q)

-- | /n/ = length, /m/ equals multiplier, /r/ = initial ratio.
--
-- > tun_seq 5 (3/2) 1 == [1/1,3/2,9/8,27/16,81/64]
tun_seq :: Int -> Rational -> Rational -> [Rational]
tun_seq n m = take n . iterate (rat_mul m)

-- | 'T.ratio_to_cents' rounded to nearest multiple of 100.
--
-- > map ratio_to_pc [1,4/3,3/2,2] == [0,5,7,12]
ratio_to_pc :: Rational -> Int
ratio_to_pc = round . (/ 100) . T.ratio_to_cents

all_pairs :: [t] -> [u] -> [(t,u)]
all_pairs p q = [(x,y) | x <- p, y <- q]

euler_align_rat :: T.T2 Rational -> T.T3 [Rational] -> T.T2 [T.T2 Rational]
euler_align_rat (r1,r2) (l1,l2,l3) =
    let f r (p,q) = rat_mul p r == q
    in (filter (f r1) (all_pairs l2 l1)
       ,filter (f r2) (all_pairs l3 l2))

-- | Pretty printer for pitch class.
--
-- > unwords (map pc_pp [0..11]) == "C♮ C♯ D♮ E♭ E♮ F♮ F♯ G♮ A♭ A♮ B♭ B♮"
pc_pp :: Integral i => i -> String
pc_pp x =
    let Just (n,a) = T.pc_to_note_alteration_ks x
    in [T.note_pp n,T.alteration_symbol a]

-- > rat_label 1 == "C♮\\n1:1"
rat_label :: Rational -> String
rat_label r = concat [pc_pp (ratio_to_pc r),"\\n",T.ratio_pp r]

-- > rat_id (5/4) == "R_5_4"
rat_id :: Rational-> String
rat_id r = "R_" ++ show (numerator r) ++ "_" ++ show (denominator r)

edge_label :: (Rational, Rational) -> String
edge_label (p,q) = concat ["   (",T.ratio_pp (rat_div p q),")"]

-- | Zip start-middle-end.
--
-- > zip_sme (0,1,2) "abcd" == [(0,'a'),(1,'b'),(1,'c'),(2,'d')]
zip_sme :: (t,t,t) -> [u] -> [(t,u)]
zip_sme (s,m,e) xs =
    case xs of
      x0:x1:xs' -> (s,x0) : (m,x1) : T.at_last (\x -> (m,x)) (\x -> (e,x)) xs'
      _ -> error "zip_sme: not SME list"

euler_plane_to_dot :: (t -> String,t -> String,(t,t) -> String) -> ([[t]],[(t,t)]) -> [String]
euler_plane_to_dot (n_id,n_pp,e_pp) (h,v) =
    let mk_lab_term x =concat [" [label=\"",x,"\"];"]
        node_to_dot x = concat [n_id x,mk_lab_term (n_pp x)]
        subgraph_edges x = intercalate " -- " (map n_id x) ++ ";"
        edge_to_dot (lhs,rhs) = concat [n_id lhs," -- ",n_id rhs,mk_lab_term (e_pp (lhs,rhs))]
        subgraphs_to_dot (ty,x) = concat ["{rank=",ty,"; ",unwords (map n_id x),"}"]
    in ["graph g {"
       ,"graph [layout=\"dot\",rankdir=\"TB\",nodesep=0.5];"
       ,"edge [fontsize=\"8\",fontname=\"century schoolbook\"];"
       ,"node [shape=\"plaintext\",fontsize=\"10\",fontname=\"century schoolbook\"];"] ++
       map node_to_dot (concat h) ++
       map subgraph_edges h ++
       map edge_to_dot v ++
       map subgraphs_to_dot (zip_sme ("min","same","max") h) ++
       ["}"]

{-

let j5 =
    let {l1 = tun_seq 3 (3%2) (5%3)
        ;l2 = tun_seq 5 (3%2) (16%9)
        ;l3 = tun_seq 4 (3%2) (64%45)
        ;(c1,c2) = euler_align_rat (5%8,5%4) (l1,l2,l3)}
    in ([l1,l2,l3],c1 ++ c2)

let j7 =
    let {l1 = tun_seq 4 (3%2) (5%4)
        ;l2 = tun_seq 5 (3%2) (4%3)
        ;l3 = tun_seq 3 (3%2) (14%9)
        ;(c1,c2) = euler_align_rat (5%4,4%7) (l1,l2,l3)}
    in ([l1,l2,l3],c1 ++ c2)

writeFile "/tmp/g.dot" (unlines (euler_plane_to_dot (rat_id,rat_label,edge_label) j5))
writeFile "/tmp/g.dot" (unlines (euler_plane_to_dot (rat_id,rat_label,edge_label) j7))

-}

