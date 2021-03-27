{- | LCF (Lederberg/Coxeter/Frucht) notation

The notation only applies to Hamiltonian graphs, since it achieves its
symmetry and conciseness by placing a Hamiltonian cycle in a circular
embedding and then connecting specified pairs of nodes with edges. (EW)

-}
module Music.Theory.Graph.LCF where

import Data.Complex {- base -}
import Data.List {- base -}

import qualified Music.Theory.Graph.Type as T {- hmt -}

-- | LCF notation (/l/,/k/). ([3,-3],4) is the cubical graph.
type LCF = ([Int],Int)

-- | Real, alias for 'Double'
type R = Double

-- | Sequence, ie. /l/ /k/ times.
lcf_seq :: LCF -> [Int]
lcf_seq (l,k) = concat (replicate k l)

-- | Length of 'lcf_seq', ie. |l|k
lcf_degree :: LCF -> Int
lcf_degree (l,k) = length l * k

-- | 'LCF' to 'T.EDG' (an edge list)
lcf_to_edg :: LCF -> T.EDG
lcf_to_edg (l,k) =
  let v_n = length l * k
      add i j = (i + j) `mod` v_n
      v = [0 .. v_n - 1]
  in ((v_n,v_n + (v_n `div` 2))
     ,concat [[(i,i `add` 1) | i <- v]
             ,nub (sort (zipWith (curry T.e_sort) v (zipWith add v (lcf_seq (l,k)))))])

-- | LCF edge-list to graph labeled with circular co-ordinates.
edg_circ_gr :: R -> T.EDG -> T.LBL (R,R) ()
edg_circ_gr rad ((n,_),e) =
  let polar_to_rectangular (mg,ph) = let c = mkPolar mg ph in (realPart c,imagPart c)
      ph_incr = (2 * pi) / fromIntegral n
      v = zip [0 .. n - 1] (map (curry polar_to_rectangular rad) [0, ph_incr ..])
  in (v,zip e (repeat ()))

{- | LCF graph set given at <http://mathworld.wolfram.com/LCFNotation.html>

> length lcf_mw_set == 57
> length (nub (map snd lcf_mw_set)) == 57 -- IE. UNIQ
-}
lcf_mw_set :: [(String, LCF)]
lcf_mw_set =
  [("Tetrahedral graph",([2,-2],2)) -- ([2],4)
  ,("Utility graph",([3],6)) -- ([3,-3],3)
  ,("3-prism graph",([-3,-2,2],2))
  ,("Cubical graph",([3,-3],4))
  ,("Wagner graph",([4],8))
  ,("3-matchstick graph",([-2,-2,2,2],2))
  ,("4-Möbius ladder",([-4],8))
  ,("5-Möbius ladder",([-5],10))
  ,("5-prism graph",([-5,3,-4,4,-3],2))
  ,("Bidiakis cube",([6,4,-4],4))
  ,("Franklin graph",([5,-5],6))
  ,("Frucht graph",([-5,-2,-4,2,5,-2,2,5,-2,-5,4,2],1))
  ,("Truncated tetrahedral graph",([2,6,-2],4))
  ,("Generalized Petersen graph (6,2)",([-5,2,4,-2,-5,4,-4,5,2,-4,-2,5],1))
  ,("6-Möbius ladder",([-6],12))
  ,("6-prism graph",([-3,3],6))
  ,("Heawood graph",([5,-5],7))
  ,("Generalized Petersen graph (7,2)",([-7,-5,4,-6,-5,4,-4,-7,4,-4,5,6,-4,5],1))
  ,("7-Möbius ladder",([-7],14))
  ,("7-prism graph",([-7,5,3,-6,6,-3,-5],2))
  ,("Cubic vertex-transitive graph Ct19",([-7,7],8))
  ,("Möbius-Kantor graph",([5,-5],8))
  ,("8-Möbius ladder",([-8],16))
  ,("8-prism graph",([-3,3],8))
  ,("Pappus graph",([5,7,-7,7,-7,-5],3))
  ,("Cubic vertex-transitive graph Ct20",([-7,7],9)) -- ([5,-5],9)
  ,("Cubic vertex-transitive graph Ct23",([-9,-2,2],6))
  ,("Generalized Petersen graph (9,2)",([-9,-8,-4,-9,4,8],3))
  ,("Generalized Petersen graph (9,3)",([-9,-6,2,5,-2,-9,5,-9,-5,-9,2,-5,-2,6,-9,2,-9,-2],1))
  ,("9-Möbius ladder",([-9],18))
  ,("9-prism graph",([-9,7,5,3,-8,8,-3,-5,-7],2))
  ,("Desargues graph",([5,-5,9,-9],5))
  ,("Dodecahedral graph",([10,7,4,-4,-7,10,-4,7,-7,4],2))
  ,("Cubic vertex-transitive graph Ct25",([-7,7],10))
  ,("Cubic vertex-transitive graph Ct28",([-6,-6,6,6],5))
  ,("Cubic vertex-transitive graph Ct29",([-9,9],10))
  ,("Generalized Petersen graph (10,4)",([-10,-7,5,-5,7,-6,-10,-5,5,6],2))
  ,("Largest cubic nonplanar graph with diameter 3",([-10,-7,-5,4,7,-10,-7,-4,5,7,-10,-7,6,-5,7,-10,-7,5,-6,7],1))
  ,("10-Möbius ladder",([-10],20))
  ,("10-prism graph",([-3,3],10))
  ,("McGee graph",([12,7,-7],8))
  ,("Truncated cubical graph",([2,9,-2,2,-9,-2],4))
  ,("Truncated octahedral graph",([3,-7,7,-3],6))
  ,("Nauru graph",([5,-9,7,-7,9,-5],4))
  ,("F26A graph",([-7,7],13))
  ,("Tutte-Coxeter graph",([-13,-9,7,-7,9,13],5))
  ,("Dyck graph",([5,-5,13,-13],8))
  ,("Gray graph",([-25,7,-7,13,-13,25],9))
  ,("Truncated dodecahedral graph",([30,-2,2,21,-2,2,12,-2,2,-12,-2,2,-21,-2,2,30,-2,2,-12,-2,2,21,-2,2,-21,-2,2,12,-2,2],2))
  ,("Harries graph",([-29,-19,-13,13,21,-27,27,33,-13,13,19,-21,-33,29],5))
  ,("Harries-Wong graph",([9,25,31,-17,17,33,9,-29,-15,-9,9,25,-25,29,17,-9,9,-27,35,-9,9,-17,21,27,-29,-9,-25,13,19,-9,-33,-17,19,-31,27,11,-25,29,-33,13,-13,21,-29,-21,25,9,-11,-19,29,9,-27,-19,-13,-35,-9,9,17,25,-9,9,27,-27,-21,15,-9,29,-29,33,-9,-25],1))
  ,("Balaban 10-cage",([-9,-25,-19,29,13,35,-13,-29,19,25,9,-29,29,17,33,21,9,-13,-31,-9,25,17,9,-31,27,-9,17,-19,-29,27,-17,-9,-29,33,-25,25,-21,17,-17,29,35,-29,17,-17,21,-25,25,-33,29,9,17,-27,29,19,-17,9,-27,31,-9,-17,-25,9,31,13,-9,-21,-33,-17,-29,29],1))
  ,("Foster graph",([17,-9,37,-37,9,-17],15))
  ,("Biggs-Smith graph",([16,24,-38,17,34,48,-19,41,-35,47,-20,34,-36,21,14,48,-16,-36,-43,28,-17,21,29,-43,46,-24,28,-38,-14,-50,-45,21,8,27,-21,20,-37,39,-34,-44,-8,38,-21,25,15,-34,18,-28,-41,36,8,-29,-21,-48,-28,-20,-47,14,-8,-15,-27,38,24,-48,-18,25,38,31,-25,24,-46,-14,28,11,21,35,-39,43,36,-38,14,50,43,36,-11,-36,-24,45,8,19,-25,38,20,-24,-14,-21,-8,44,-31,-38,-28,37],1))
  ,("Balaban 11-cage",([44,26,-47,-15,35,-39,11,-27,38,-37,43,14,28,51,-29,-16,41,-11,-26,15,22,-51,-35,36,52,-14,-33,-26,-46,52,26,16,43,33,-15,17,-53,23,-42,-35,-28,30,-22,45,-44,16,-38,-16,50,-55,20,28,-17,-43,47,34,-26,-41,11,-36,-23,-16,41,17,-51,26,-33,47,17,-11,-20,-30,21,29,36,-43,-52,10,39,-28,-17,-52,51,26,37,-17,10,-10,-45,-34,17,-26,27,-21,46,53,-10,29,-50,35,15,-47,-29,-41,26,33,55,-17,42,-26,-36,16],1))
  ,("Ljubljana graph",([47,-23,-31,39,25,-21,-31,-41,25,15,29,-41,-19,15,-49,33,39,-35,-21,17,-33,49,41,31,-15,-29,41,31,-15,-25,21,31,-51,-25,23,9,-17,51,35,-29,21,-51,-39,33,-9,-51,51,-47,-33,19,51,-21,29,21,-31,-39],2))
  ,("Tutte 12-cage",([17,27,-13,-59,-35,35,-11,13,-53,53,-27,21,57,11,-21,-57,59,-17],7))]

-- Local Variables:
-- truncate-lines:t
-- End:
