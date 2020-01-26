-- | Erv Wilson, archives.
module Music.Theory.Tuning.Wilson where

import Data.List {- base -}

import qualified Music.Theory.Tuning as T {- hmt -}

type R = Rational

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

-- | <http://anaphoria.com/xen456.pdf> P.9
ew_xen456_9 :: [R]
ew_xen456_9 = (nub . sortOn T.fold_ratio_to_octave_err . concatMap m3_gen_unfold) ew_xen456_9_gen

-- | <http://anaphoria.com/Pelogflute.pdf> P.2
ew_pf_2 :: Fractional n => [n]
ew_pf_2 = [1,16/15,64/55,5/4,4/3,16/11,8/5,128/75,20/11]

