module Music.Theory.Duration.RQ.Tied where

import Data.Maybe
import Music.Theory.Duration.Annotation
import Music.Theory.Duration.RQ

-- * Tied RQ ('RQ_T')

data Tied = T | F deriving (Eq,Show)
type RQ_T = (RQ,Tied)

-- | 'RQ' field of 'RQ_T'.
rqt_rq :: RQ_T -> RQ
rqt_rq = fst

-- | 'Tied' field of 'RQ_T'.
rqt_tied :: RQ_T -> Tied
rqt_tied = snd

-- | Is 'RQ_T' tied right.
is_tied_right :: RQ_T -> Bool
is_tied_right (_,x) = x == T

-- | 'RQ_T' variant of 'rq_un_tuplet'.
--
-- > rqt_un_tuplet (3,2) (1,T) == (3/2,T)
--
-- > let f = rqt_un_tuplet (7,4)
-- > in map f [(2/7,F),(4/7,T),(1/7,F)] == [(1/2,F),(1,T),(1/4,F)]
rqt_un_tuplet :: (Integer,Integer) -> RQ_T -> RQ_T
rqt_un_tuplet i (d,t) = (rq_un_tuplet i d,t)

-- | Transform 'RQ' to untied 'RQ_T'.
--
-- > rq_rqt 3 == (3,F)
rq_rqt :: RQ -> RQ_T
rq_rqt n = (n,F)

-- | Apply /f/ at all but last element, and /g/ at last element.
--
-- > at_last (* 2) negate [1..4] == [2,4,6,-4]
at_last :: (a -> b) -> (a -> b) -> [a] -> [b]
at_last f g x =
    case x of
      [] -> []
      [i] -> [g i]
      i:x' -> f i : at_last f g x'

-- | Tie last element only of list of 'RQ'.
--
-- > rq_tie_last [1,2,3] == [(1,F),(2,F),(3,T)]
rq_tie_last :: [RQ] -> [RQ_T]
rq_tie_last = at_last rq_rqt (\d -> (d,T))

-- | Transform a list of 'RQ_T' to a list of 'Duration_A'.  The flag
-- indicates if the initial value is tied left.
--
-- > rqt_to_duration_a False [(1,T),(1/4,T),(3/4,F)]
rqt_to_duration_a :: Bool -> [RQ_T] -> [Duration_A]
rqt_to_duration_a z x =
    let rt = map is_tied_right x
        lt = z : rt
        f p e = if p then Just e else Nothing
        g (r,l) = catMaybes [f r Tie_Right,f l Tie_Left]
        h = rq_to_duration_err (show ("rqt_to_duration_a",z,x)) . rqt_rq
    in zip (map h x) (map g (zip rt lt))

-- | 'RQ_T' variant of 'rq_can_notate'.
rqt_can_notate :: [RQ_T] -> Bool
rqt_can_notate = rq_can_notate . map rqt_rq

-- | 'RQ_T' variant of 'rq_to_cmn'.
--
-- > rqt_to_cmn (5,T) == Just ((4,T),(1,T))
-- > rqt_to_cmn (5/4,T) == Just ((1,T),(1/4,T))
-- > rqt_to_cmn (5/7,F) == Just ((4/7,T),(1/7,F))
rqt_to_cmn :: RQ_T -> Maybe (RQ_T,RQ_T)
rqt_to_cmn (k,t) =
    let f (i,j) = ((i,T),(j,t))
    in fmap f (rq_to_cmn k)

-- | List variant of 'rqt_to_cmn'.
--
-- > rqt_to_cmn_l (5,T) == [(4,T),(1,T)]
rqt_to_cmn_l :: RQ_T -> [RQ_T]
rqt_to_cmn_l x = maybe [x] (\(i,j) -> [i,j]) (rqt_to_cmn x)

-- | 'concatMap' 'rqt_to_cmn_l'.
--
-- > rqt_set_to_cmn [(1,T),(5/4,F)] == [(1,T),(1,T),(1/4,F)]
rqt_set_to_cmn :: [RQ_T] -> [RQ_T]
rqt_set_to_cmn = concatMap rqt_to_cmn_l
