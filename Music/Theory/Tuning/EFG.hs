-- | Euler-Fokker genus <http://www.huygens-fokker.org/microtonality/efg.html>
module Music.Theory.Tuning.EFG where

import Data.List {- base -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Set.List as T {- hmt -}

import Music.Theory.Tuning {- hmt -}

-- | Normal form, value with occurences count (ie. exponent in notation above).
type EFG i = [(i,Int)]

-- | Degree of EFG, ie. sum of exponents.
--
-- > efg_degree [(3,3),(7,2)] == 3 + 2
efg_degree :: EFG i -> Int
efg_degree = sum . map snd

-- | Number of tones of EFG, ie. product of increment of exponents.
--
-- > efg_tones [(3,3),(7,2)] == (3 + 1) * (2 + 1)
efg_tones :: EFG i -> Int
efg_tones = product . map ((+ 1) . snd)

-- | Collate a genus given as a multiset into standard form, ie. histogram.
--
-- > efg_collate [3,3,3,7,7] == [(3,3),(7,2)]
efg_collate :: Ord i => [i] -> EFG i
efg_collate = T.histogram . sort

{- | Factors of EFG given with co-ordinate of grid location.

> efg_factors [(3,3)]

> let r = [([0,0],[]),([0,1],[7]),([0,2],[7,7])
>         ,([1,0],[3]),([1,1],[3,7]),([1,2],[3,7,7])
>         ,([2,0],[3,3]),([2,1],[3,3,7]),([2,2],[3,3,7,7])
>         ,([3,0],[3,3,3]),([3,1],[3,3,3,7]),([3,2],[3,3,3,7,7])]

> efg_factors [(3,3),(7,2)] == r

-}
efg_factors :: EFG i -> [([Int],[i])]
efg_factors efg =
    let k = map (\(_,n) -> [0 .. n]) efg
        k' = if length efg == 1
             then concatMap (map return) k
             else T.nfold_cartesian_product k
        z = map fst efg
        f ix = (ix,concat (zipWith (\n m -> replicate n (z !! m)) ix [0..]))
    in map f k'

{- | Ratios of EFG, taking /n/ as the 1:1 ratio, with indices, folded into one octave.

> import Data.List
> let r = sort $ map snd $ efg_ratios 7 [(3,3),(7,2)]
> r == [1/1,9/8,8/7,9/7,21/16,189/128,3/2,27/16,12/7,7/4,27/14,63/32]
> map (round . ratio_to_cents) r == [0,204,231,435,471,675,702,906,933,969,1137,1173]

      0:         1/1          C          0.000 cents
      1:         9/8          D        203.910 cents
      2:         8/7          D+       231.174 cents
      3:         9/7          E+       435.084 cents
      4:        21/16         F-       470.781 cents
      5:       189/128        G-       674.691 cents
      6:         3/2          G        701.955 cents
      7:        27/16         A        905.865 cents
      8:        12/7          A+       933.129 cents
      9:         7/4          Bb-      968.826 cents
     10:        27/14         B+      1137.039 cents
     11:        63/32         C-      1172.736 cents
     12:         2/1          C       1200.000 cents

> let r' = sort $ map snd $ efg_ratios 5 [(5,2),(7,3)]
> r' == [1/1,343/320,35/32,49/40,5/4,343/256,7/5,49/32,8/5,1715/1024,7/4,245/128]
> map (round . ratio_to_cents) r' == [0,120,155,351,386,506,583,738,814,893,969,1124]

> let r'' = sort $ map snd $ efg_ratios 3 [(3,1),(5,1),(7,1)]
> r'' == [1/1,35/32,7/6,5/4,4/3,35/24,5/3,7/4]
> map (round . ratio_to_cents) r'' == [0,155,267,386,498,653,884,969]

> let c0 = [0,204,231,435,471,675,702,906,933,969,1137,1173,1200]
> let c1 = [0,120,155,351,386,506,583,738,814,893,969,1124,1200]
> let c2 = [0,155,267,386,498,653,884,969,1200]
> let f (c',y) = map (\x -> (x,y,x,y + 10)) c'
> map f (zip [c0,c1,c2] [0,20,40])

-}
efg_ratios :: Real r => Rational -> EFG r -> [([Int],Rational)]
efg_ratios n =
    let to_r = fold_ratio_to_octave_err . (/ n) . toRational . product
        f (ix,i) = (ix,to_r i)
    in map f . efg_factors

{- | Generate a line drawing, as a set of (x0,y0,x1,y1) 4-tuples.
     h=row height, m=distance of vertical mark from row edge, k=distance between rows

> let e = [[3,3,3],[3,3,5],[3,5,5],[3,5,7],[3,7,7],[5,5,5],[5,5,7],[3,3,7],[5,7,7],[7,7,7]]
> let e = [[3,3,3],[5,5,5],[7,7,7],[3,3,5],[3,5,5],[5,5,7],[5,7,7],[3,7,7],[3,3,7],[3,5,7]]
> let e' = map efg_collate e
> efg_diagram_set (round,25,4,75) e'

-}
efg_diagram_set :: (Enum n,Real n) => (Cents -> n,n,n,n) -> [EFG n] -> [(n,n,n,n)]
efg_diagram_set (to_f,h,m,k) e =
    let f = (++ [1200]) . sort . map (to_f . ratio_to_cents . snd) . efg_ratios 1
        g (c,y) = let y' = y + h
                      b = [(0,y,1200,y),(0,y',1200,y')]
                  in b ++ map (\x -> (x,y + m,x,y' - m)) c
    in concatMap g (zip (map f e) [0,k ..])
