{- | 'Rq' sub-divisions. -}
module Music.Theory.Duration.Rq.Division where

import Data.List.Split {- split -}
import Data.Ratio {- base -}

import qualified Music.Theory.List as List {- hmt-base -}
import qualified Music.Theory.Permutations.List as Permutations.List {- hmt-base -}

import Music.Theory.Duration.Rq
import Music.Theory.Duration.Rq.Tied

{- | Divisions of /n/ 'Rq' into /i/ equal parts grouped as /j/.
A quarter and eighth note triplet is written @(1,1,[2,1],False)@.
-}
type Rq_Div = (Rational,Integer,[Integer],Tied_Right)

{- | Variant of 'Rq_Div' where /n/ is @1@. -}
type Rq1_Div = (Integer,[Integer],Tied_Right)

{- | Lift 'Rq1_Div' to 'Rq_Div'. -}
rq1_div_to_rq_div :: Rq1_Div -> Rq_Div
rq1_div_to_rq_div (i,j,k) = (1,i,j,k)

{- | Verify that grouping /j/ sums to the divisor /i/. -}
rq_div_verify :: Rq_Div -> Bool
rq_div_verify (_,n,m,_) = n == sum m

rq_div_mm_verify :: Int -> [Rq_Div] -> [(Integer,[Rq])]
rq_div_mm_verify n x =
    let q = map (sum . fst . rq_div_to_rq_set_t) x
    in zip [1..] (chunksOf n q)

{- | Translate from 'Rq_Div' to a sequence of 'Rq' values.

>>> rq_div_to_rq_set_t (1,5,[1,3,1],True)
([1 % 5,3 % 5,1 % 5],True)

>>> rq_div_to_rq_set_t (1/2,6,[3,1,2],False)
([1 % 4,1 % 12,1 % 6],False)
-}
rq_div_to_rq_set_t :: Rq_Div -> ([Rq],Tied_Right)
rq_div_to_rq_set_t (n,k,d,t) =
    let q = map ((* n) . (% k)) d
    in (q,t)

{- | Translate from result of 'rq_div_to_rq_set_t' to seqeunce of 'Rq_Tied'.

>>> rq_set_t_to_rqt ([1/5,3/5,1/5],True)
[(1 % 5,False),(3 % 5,False),(1 % 5,True)]
-}
rq_set_t_to_rqt :: ([Rq],Tied_Right) -> [Rq_Tied]
rq_set_t_to_rqt (x,t) = List.at_last (\i -> (i,False)) (\i -> (i,t)) x

{- | Transform sequence of 'Rq_Div' into sequence of 'Rq', discarding any final tie.

>>> rq_div_seq_rq [(1,5,[1,3,1],True),(1/2,6,[3,1,2],True)]
[1 % 5,3 % 5,9 % 20,1 % 12,1 % 6]
-}
rq_div_seq_rq :: [Rq_Div] -> [Rq]
rq_div_seq_rq =
    let f i qq = case qq of
                  [] -> maybe [] return i
                  q:qq' -> let (r,t) = rq_div_to_rq_set_t q
                               r' = maybe r (\j -> List.at_head (+ j) id r) i
                           in if t
                              then let (r'',i') = List.separate_last r'
                                   in r'' ++ f (Just i') qq'
                              else r' ++ f Nothing qq'
    in f Nothing

{- | Partitions of an 'Integral' that sum to /n/.
This includes the two 'trivial paritions, into a set /n/ @1@, and a set of @1@ /n/.

>>> partitions_sum 4
[[1,1,1,1],[2,1,1],[2,2],[3,1],[4]]

>>> map (length . partitions_sum) [9..15]
[30,42,56,77,101,135,176]
-}
partitions_sum :: Integral i => i -> [[i]]
partitions_sum n =
    let f p = if null p then 0 else head p
    in case n of
         0 -> [[]]
         _ -> [x:y | x <- [1..n], y <- partitions_sum (n - x), x >= f y]

{- | The 'multiset_permutations' of 'partitions_sum'.

>>> map (length . partitions_sum_p) [9..12]
[256,512,1024,2048]
-}
partitions_sum_p :: Integral i => i -> [[i]]
partitions_sum_p = concatMap Permutations.List.multiset_permutations . partitions_sum

{- | The set of all 'Rq1_Div' that sum to /n/, a variant on
-- 'partitions_sum_p'.

>>> map (length . rq1_div_univ) [3..5]
[8,16,32]

>>> map (length . rq1_div_univ) [9..12]
[512,1024,2048,4096]
-}
rq1_div_univ :: Integer -> [Rq1_Div]
rq1_div_univ n =
    let f l = [(n,l,k) | k <- [False,True]]
    in concatMap f (partitions_sum_p n)
