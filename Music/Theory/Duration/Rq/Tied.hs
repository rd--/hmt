-- | 'Rq' values with /tie right/ qualifier.
module Music.Theory.Duration.Rq.Tied where

import Data.Maybe {- base -}

import Music.Theory.List {- hmt-base -}

import Music.Theory.Duration {- hmt -}
import qualified Music.Theory.Duration.Annotation as Annotation {- hmt -}
import Music.Theory.Duration.Rq {- hmt -}

-- | Boolean.
type Tied_Right = Bool

-- | 'Rq' with /tie right/.
type Rq_Tied = (Rq,Tied_Right)

-- | If Rq_Tied is not tied, get Rq.
rqt_to_rq :: Rq_Tied -> Maybe Rq
rqt_to_rq (rq,x) = if x then Nothing else Just rq

-- | Erroring variant of rqt_to_rq.
rqt_to_rq_err :: Rq_Tied -> Rq
rqt_to_rq_err = fromMaybe (error "rqt_to_rq") . rqt_to_rq

-- | Construct 'Rq_Tied'.
rqt :: Tied_Right -> Rq -> Rq_Tied
rqt t d = (d,t)

-- | 'Rq' field of 'Rq_Tied'.
rqt_rq :: Rq_Tied -> Rq
rqt_rq = fst

-- | 'Tied' field of 'Rq_Tied'.
rqt_tied :: Rq_Tied -> Tied_Right
rqt_tied = snd

-- | Is 'Rq_Tied' tied right.
is_tied_right :: Rq_Tied -> Bool
is_tied_right = snd

{- | 'Rq_Tied' variant of 'rq_un_tuplet'.

>>> rqt_un_tuplet (3,2) (1,True)
(3 % 2,True)

>>> let f = rqt_un_tuplet (7,4)
>>> map f [(2/7,False),(4/7,True),(1/7,False)]
[(1 % 2,False),(1 % 1,True),(1 % 4,False)]
-}
rqt_un_tuplet :: (Integer,Integer) -> Rq_Tied -> Rq_Tied
rqt_un_tuplet i (d,t) = (rq_un_tuplet i d,t)

{- | Transform 'Rq' to untied 'Rq_Tied'.

>>> rq_rqt 3
(3 % 1,False)
-}
rq_rqt :: Rq -> Rq_Tied
rq_rqt n = (n,False)

{- | Tie last element only of list of 'Rq'.

>>> rq_tie_last [1,2,3]
[(1 % 1,False),(2 % 1,False),(3 % 1,True)]
-}
rq_tie_last :: [Rq] -> [Rq_Tied]
rq_tie_last = at_last rq_rqt (\d -> (d,True))

{- | Transform a list of 'Rq_Tied' to a list of 'Duration_A'.
The flag indicates if the initial value is tied left.

>>> rqt_to_duration_a False [(1,True),(1/4,True),(3/4,False)]
[(Duration {division = 4, dots = 0, multiplier = 1 % 1},[Tie_Right]),(Duration {division = 16, dots = 0, multiplier = 1 % 1},[Tie_Right,Tie_Left]),(Duration {division = 8, dots = 1, multiplier = 1 % 1},[Tie_Left])]
-}
rqt_to_duration_a :: Bool -> [Rq_Tied] -> [Annotation.Duration_A]
rqt_to_duration_a z x =
    let rt = map is_tied_right x
        lt = z : rt
        f p e = if p then Just e else Nothing
        g r l = catMaybes [f r Annotation.Tie_Right,f l Annotation.Tie_Left]
        h = rq_to_duration_err (show ("rqt_to_duration_a",z,x)) 2 . rqt_rq
    in zip (map h x) (zipWith g rt lt)

{- | 'Rq_Tied' variant of 'rq_can_notate'. -}
rqt_can_notate :: Dots -> [Rq_Tied] -> Bool
rqt_can_notate k = rq_can_notate k  . map rqt_rq

{- | 'Rq_Tied' variant of 'rq_to_cmn'.

>>> rqt_to_cmn (5,True)
Just ((4 % 1,True),(1 % 1,True))

>>> rqt_to_cmn (5/4,True)
Just ((1 % 1,True),(1 % 4,True))

>>> rqt_to_cmn (5/7,False)
Just ((4 % 7,True),(1 % 7,False))
-}
rqt_to_cmn :: Rq_Tied -> Maybe (Rq_Tied,Rq_Tied)
rqt_to_cmn (k,t) =
    let f (i,j) = ((i,True),(j,t))
    in fmap f (rq_to_cmn k)

{- | List variant of 'rqt_to_cmn'.

>>> rqt_to_cmn_l (5,True)
[(4 % 1,True),(1 % 1,True)]
-}
rqt_to_cmn_l :: Rq_Tied -> [Rq_Tied]
rqt_to_cmn_l x = maybe [x] (\(i,j) -> [i,j]) (rqt_to_cmn x)

{- | 'concatMap' 'rqt_to_cmn_l'.

>>> rqt_set_to_cmn [(1,True),(5/4,False)]
[(1 % 1,True),(1 % 1,True),(1 % 4,False)]

>>> rqt_set_to_cmn [(1/5,True),(1/20,False),(1/2,False),(1/4,True)]
[(1 % 5,True),(1 % 20,False),(1 % 2,False),(1 % 4,True)]
-}
rqt_set_to_cmn :: [Rq_Tied] -> [Rq_Tied]
rqt_set_to_cmn = concatMap rqt_to_cmn_l
