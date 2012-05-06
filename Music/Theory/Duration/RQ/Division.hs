-- | 'RQ' sub-divisions.
module Music.Theory.Duration.RQ.Division where

import Data.List.Split {- split -}
import Data.Ratio

import Music.Theory.Duration.RQ
import Music.Theory.Duration.RQ.Tied
import Music.Theory.List

-- | Divisions of /n/ 'RQ' into /i/ equal parts grouped as /j/.
-- A quarter note and eighth note triplet is written @(1,1,[2,1],False)@.
type RQ_Div = (Rational,Integer,[Integer],Tied_Right)

-- | Variant of 'RQ_Div' where /n/ is @1@.
type RQ1_Div = (Integer,[Integer],Tied_Right)

-- | Lift 'RQ1_Div' to 'RQ_Div'.
rq1_div_to_rq_div :: RQ1_Div -> RQ_Div
rq1_div_to_rq_div (i,j,k) = (1,i,j,k)

-- | Verify that grouping /j/ sums to the divisor /i/.
rq_div_verify :: RQ_Div -> Bool
rq_div_verify (_,n,m,_) = n == sum m

rq_div_mm_verify :: Int -> [RQ_Div] -> [(Integer,[RQ])]
rq_div_mm_verify n x =
    let q = map (sum . fst . rq_div_to_rq_set_t) x
    in zip [1..] (splitEvery n q)

-- | Translate from 'RQ_Div' to a sequence of 'RQ' values.
--
-- > rq_div_to_rq_set_t (1,5,[1,3,1],True) == ([1/5,3/5,1/5],True)
-- > rq_div_to_rq_set_t (1/2,6,[3,1,2],False) == ([1/4,1/12,1/6],False)
rq_div_to_rq_set_t :: RQ_Div -> ([RQ],Tied_Right)
rq_div_to_rq_set_t (n,k,d,t) =
    let q = map ((* n) . (% k)) d
    in (q,t)

-- | Translate from result of 'rq_div_to_rq_set_t' to seqeunce of 'RQ_T'.
--
-- > rq_set_t_to_rqt ([1/5,3/5,1/5],True) == [(1/5,_f),(3/5,_f),(1/5,_t)]
rq_set_t_to_rqt :: ([RQ],Tied_Right) -> [RQ_T]
rq_set_t_to_rqt (x,t) = at_last (\i -> (i,False)) (\i -> (i,t)) x

-- > at_head negate [1..5] == [-1,2,3,4,5]
at_head :: (a -> a) -> [a] -> [a]
at_head f x =
    case x of
      e:x' -> f e : x'
      _ -> x

-- > sep_last [1..5] == ([1..4],5)
sep_last :: [a] -> ([a],a)
sep_last x =
    let e:x' = reverse x
    in (reverse x',e)

-- | Transform sequence of 'RQ_Div' into sequence of 'RQ'.
--
-- > let q = [(1,5,[1,3,1],True),(1/2,6,[3,1,2],False)]
-- > in rq_div_seq_rq q == [1/5,3/5,9/20,1/12,1/6]
rq_div_seq_rq :: [RQ_Div] -> [RQ]
rq_div_seq_rq =
    let f i qq = case qq of
                  q:qq' -> let (r,t) = rq_div_to_rq_set_t q
                               r' = case i of
                                      Just j -> at_head (+ j) r
                                      Nothing -> r
                           in if t
                              then let (r'',i') = sep_last r'
                                   in r'' ++ f (Just i') qq'
                              else r' ++ f Nothing qq'
                  _ -> []
    in f Nothing
