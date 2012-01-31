-- | Notation of a sequence of 'RQ' values as annotated 'Duration' values.
--
-- 1. separate into measures, adding tie annotations as required.
-- 2. separate each measure into pulses
-- 3. simplify each measure
module Music.Theory.Duration.Sequence.Notate where

import Control.Monad
import Data.List
import Data.Maybe
--import Data.Ratio
import Music.Theory.Duration
import Music.Theory.Duration.Annotation
import Music.Theory.Duration.RQ
import Music.Theory.Time_Signature

-- * Lists

-- | If all elements of the list are @Just a@, then gives @Just [a]@
-- else gives 'Nothing'.
--
-- > all_just (map Just [1..3]) == Just [1..3]
-- > all_just [Just 1,Nothing,Just 3] == Nothing
all_just :: [Maybe a] -> Maybe [a]
all_just x =
    case x of
      [] -> Just []
      Just i:x' -> fmap (i :) (all_just x')
      Nothing:_ -> Nothing

-- | Apply /f/ at all but last element, and /g/ at last element.
--
-- > at_last (* 2) negate [1..4] == [2,4,6,-4]
at_last :: (a -> b) -> (a -> b) -> [a] -> [b]
at_last f g x =
    case x of
      [] -> []
      [i] -> [g i]
      i:x' -> f i : at_last f g x'

-- | Applies a /join/ function to the first two elements of the list.
-- If the /join/ function succeeds the joined element is considered
-- for further coalescing.
--
-- > coalesce (\p q -> Just (p + q)) [1..5] == [15]
--
-- > let jn p q = if even p then Just (p + q) else Nothing
-- > in coalesce jn [1..5] == map sum [[1],[2,3],[4,5]]
coalesce :: (a -> a -> Maybe a) -> [a] -> [a]
coalesce f x =
    case x of
      (p:q:x') ->
          case f p q of
            Nothing -> p : coalesce f (q : x')
            Just r -> coalesce f (r : x')
      _ -> x

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

-- * Separate

-- | Take elements while the sum of the prefix is less than or equal
-- to the indicated value.  Returns also the difference between the
-- prefix sum and the requested sum.  Note that zero elements are kept
-- left.
--
-- > take_sum 3 [2,1] == ([2,1],0,[])
-- > take_sum 3 [2,2] == ([2],1,[2])
-- > take_sum 3 [2,1,0,1] == ([2,1,0],0,[1])
-- > take_sum 3 [4] == ([],3,[4])
-- > take_sum 0 [1..5] == ([],0,[1..5])
take_sum :: (Ord a, Num a) => a -> [a] -> ([a],a,[a])
take_sum m =
    let go r n l =
            let z = (reverse r,m-n,l)
            in case l of
                 [] -> z
                 i:l' -> if i + n > m
                         then z
                         else go (i:r) (i+n) l'
    in go [] 0

-- | Split sequence such that the prefix sums to precisely /m/.  The
-- third element of the result indicates if it was required to divide
-- an element.  Not that zero elements are kept left.  If the required
-- sum is non positive, or the input list does not sum to at least the
-- required sum, gives nothing.
--
-- > split_sum 5 [2,3,1] == Just ([2,3],[1],Nothing)
-- > split_sum 5 [2,1,3] == Just ([2,1,2],[1],Just (2,1))
-- > split_sum 2 [3/2,3/2,3/2] == Just ([3/2,1/2],[1,3/2],Just (1/2,1))
-- > split_sum 6 [1..10] == Just ([1..3],[4..10],Nothing)
-- > fmap (\(a,_,c)->(a,c)) (split_sum 5 [1..]) == Just ([1,2,2],Just (2,1))
-- > split_sum 0 [1..] == Nothing
-- > split_sum 3 [1,1] == Nothing
-- > split_sum 3 [2,1,0] == Just ([2,1,0],[],Nothing)
-- > split_sum 3 [2,1,0,1] == Just ([2,1,0],[1],Nothing)
split_sum :: (Ord a, Num a) => a -> [a] -> Maybe ([a],[a],Maybe (a,a))
split_sum m l =
    let (p,n,q) = take_sum m l
    in if n == 0
       then if null p
            then Nothing
            else Just (p,q,Nothing)
       else case q of
              [] -> Nothing
              z:q' -> Just (p++[n],z-n:q',Just (n,z-n))

-- | Variant of 'split_sum' that operates at 'RQ_T' sequences.
--
-- > rqt_split_sum 5 [(3,F),(2,T),(1,F)] == Just ([(3,F),(2,T)],[(1,F)])
-- > rqt_split_sum 4 [(3,F),(2,T),(1,F)] == Just ([(3,F),(1,T)]
-- >                                             ,[(1,T),(1,F)])
rqt_split_sum :: RQ -> [RQ_T] -> Maybe ([RQ_T],[RQ_T])
rqt_split_sum d x =
    case split_sum d (map rqt_rq x) of
      Just (i,_,k) ->
          case k of
            Nothing -> Just (splitAt (length i) x)
            Just (p,q) -> let (s,(_,z):t) = splitAt (length i - 1) x
                          in Just (s ++ [(p,T)]
                                  ,((q,z) : t))
      Nothing -> Nothing

-- | Separate 'RQ_T' values in sequences summing to 'RQ' values.  This
-- is a recursive variant of 'rqt_split_sum'.  Note that is does not
-- ensure /cmn/ notation of values.
--
-- > let d = [(2,F),(2,F),(2,F)]
-- > in rqt_separate [3,3] d == Just [[(2,F),(1,T)]
-- >                                 ,[(1,F),(2,F)]]
--
-- > let d = [(5/8,F),(1,F),(3/8,F)]
-- > in rqt_separate [1,1] d == Just [[(5/8,F),(3/8,T)],[(5/8,F),(3/8,F)]]
rqt_separate :: [RQ] -> [RQ_T] -> Maybe [[RQ_T]]
rqt_separate m x =
    case (m,x) of
      ([],[]) -> Just []
      ([],_) -> Nothing
      (i:m',_) ->
          case rqt_split_sum i x of
            Just (r,x') -> fmap (r :) (rqt_separate m' x')
            Nothing -> Nothing

-- | If the input 'RQ_T' sequence cannot be notated (see
-- 'rqt_can_notate') separate into equal parts, so long as each part
-- is not less than /i/.
--
-- > rqt_separate_tuplet undefined [(1/3,F),(1/6,F)] == Nothing
-- > rqt_separate_tuplet undefined [(4/7,T),(1/7,F),(2/7,F)]
--
-- > let d = map rq_rqt [1/3,1/6,2/5,1/10]
-- > in rqt_separate_tuplet (1/8) d == Just [[(1/3,F),(1/6,F)]
-- >                                        ,[(2/5,F),(1/10,F)]]
rqt_separate_tuplet :: RQ -> [RQ_T] -> Maybe [[RQ_T]]
rqt_separate_tuplet i x =
    if rqt_can_notate x
    then Nothing
    else let j = sum (map rqt_rq x) / 2
         in if j < i then Nothing else rqt_separate [j,j] x

-- | Recursive variant of 'rqt_separate_tuplet'.
--
-- > let d = map rq_rqt [1,1/3,1/6,2/5,1/10]
-- > in rqt_tuplet_subdivide (1/8) d == [[(1/1,F)]
-- >                                    ,[(1/3,F),(1/6,F)]
-- >                                    ,[(2/5,F),(1/10,F)]]
rqt_tuplet_subdivide :: RQ -> [RQ_T] -> [[RQ_T]]
rqt_tuplet_subdivide i x =
    case rqt_separate_tuplet i x of
      Nothing -> [x]
      Just r -> concatMap (rqt_tuplet_subdivide i) r

-- | Sequence variant of 'rqt_tuplet_subdivide'.
rqt_tuplet_subdivide_seq :: RQ -> [[RQ_T]] -> [[RQ_T]]
rqt_tuplet_subdivide_seq i = concatMap (rqt_tuplet_subdivide i)

-- * Divisions

-- | Separate 'RQ' sequence into measures given by 'RQ' length.
-- Ensures 'RQ_T' are /cmn/ durations.
--
-- > to_measures_rq [3,3] [2,2,2] == Just [[(2,F),(1,T)],[(1,F),(2,F)]]
-- > to_measures_rq [3,3] [6] == Just [[(3,T)],[(3,F)]]
-- > to_measures_rq [1,1,1] [3] == Just [[(1,T)],[(1,T)],[(1,F)]]
-- > to_measures_rq [3,3] [2,2,1] == Nothing
-- > to_measures_rq [3,2] [2,2,2] == Nothing
-- > to_measures_rq [6,6] [5,5,2] == Just [[(4,T),(1,F),(1,T)]
-- >                                      ,[(4,F),(2,F)]]
to_measures_rq :: [RQ] -> [RQ] -> Maybe [[RQ_T]]
to_measures_rq m = fmap (map rqt_set_to_cmn) . rqt_separate m . map rq_rqt

-- | Separate 'RQ' sequence into measures.
--
-- > to_measures [(1,4)] [5/8,3/8] == Just [[(1/2,T),(1/8,F),(3/8,F)]]
-- > to_measures [(1,4)] [5/7,2/7] == Just [[(4/7,T),(1/7,F),(2/7,F)]]
--
-- > let {m = replicate 18 (1,4)
-- >     ;x = [3/4,2,5/4,9/4,1/4,3/2,1/2,7/4,1,5/2,11/4,3/2]}
-- > in to_measures m x == Just [[(3/4,F),(1/4,T)],[(1/1,T)]
-- >                            ,[(3/4,F),(1/4,T)],[(1/1,F)]
-- >                            ,[(1/1,T)],[(1/1,T)]
-- >                            ,[(1/4,F),(1/4,F),(1/2,T)],[(1/1,F)]
-- >                            ,[(1/2,F),(1/2,T)],[(1/1,T)]
-- >                            ,[(1/4,F),(3/4,T)],[(1/4,F),(3/4,T)]
-- >                            ,[(1/1,T)],[(3/4,F),(1/4,T)]
-- >                            ,[(1/1,T)],[(1/1,T)]
-- >                            ,[(1/2,F),(1/2,T)],[(1/1,F)]]
to_measures :: [Time_Signature] -> [RQ] -> Maybe [[RQ_T]]
to_measures m = to_measures_rq (map ts_rq m)

-- | Divide measure inot pulses of indicated 'RQ' durations.  Measure
-- must be of correct length and contain only /cmn/ durations.
m_divisions_rq :: [RQ] -> [RQ_T] -> Maybe [[RQ_T]]
m_divisions_rq z = fmap (rqt_tuplet_subdivide_seq (1/16)) . rqt_separate z

-- | Variant of 'm_divisions_rq' that determines pulse divisions from
-- 'Time_Signature'.
--
-- > let d = [(4/7,T),(1/7,F),(2/7,F)]
-- > in m_divisions (1,4) d == Just [d]
--
-- > let d = map rq_rqt [1/3,1/6,2/5,1/10]
-- > in m_divisions (1,4) d == Just [[(1/3,F),(1/6,F)],[(2/5,F),(1/10,F)]]
m_divisions :: Time_Signature -> [RQ_T] -> Maybe [[RQ_T]]
m_divisions ts = m_divisions_rq (ts_divisions ts)

-- | Composition of 'to_measures_rq' and 'm_divisions_rq', where
-- measures are initially given as sets of divisions.
--
-- > let m = [[1,1,1],[1,1,1]]
-- > in to_divisions_rq m [2,2,2] == Just [[[(1,T)],[(1,F)],[(1,T)]]
-- >                                      ,[[(1,F)],[(1,T)],[(1,F)]]]
--
-- > let d = [2/7,1/7,4/7,5/7,8/7,1,1/7]
-- > in to_divisions_rq [[1,1,1,1]] d == Just [[[(2/7,F),(1/7,F),(4/7,F)]
-- >                                           ,[(4/7,T),(1/7,F),(2/7,T)]
-- >                                           ,[(6/7,F),(1/7,T)]
-- >                                           ,[(6/7,F),(1/7,F)]]]
--
-- > let d = [2/7,1/7,4/7,5/7,1,6/7,3/7]
-- > in to_divisions_rq [[1,1,1,1]] d == Just [[[(2/7,F),(1/7,F),(4/7,F)]
-- >                                           ,[(4/7,T),(1/7,F),(2/7,T)]
-- >                                           ,[(1/2,T)]
-- >                                           ,[(3/14,F),(2/7,T)]
-- >                                           ,[(4/7,F),(3/7,F)]]]
to_divisions_rq :: [[RQ]] -> [RQ] -> Maybe [[[RQ_T]]]
to_divisions_rq m x =
    let m' = map sum m
    in case to_measures_rq m' x of
         Just y -> all_just (zipWith m_divisions_rq m y)
         Nothing -> Nothing

-- | Variant of 'to_divisions_rq' with measures given as set of
-- 'Time_Signature'.
--
-- > let d = [3/5,2/5,1/3,1/6,7/10,17/15,1/2,1/6]
-- > in to_divisions [(4,4)] d == Just [[[(3/5,F),(2/5,F)]
-- >                                    ,[(1/3,F),(1/6,F)]
-- >                                    ,[(2/5,T),(1/10,T)]
-- >                                    ,[(1/5,F),(4/5,T)]
-- >                                    ,[(1/3,F),(1/2,F),(1/6,F)]]]
--
-- > let d = [3/5,2/5,1/3,1/6,7/10,29/30,1/2,1/3]
-- > in to_divisions [(4,4)] d == Just [[[(3/5,F),(2/5,F)]
-- >                                    ,[(1/3,F),(1/6,F)]
-- >                                    ,[(2/5,T),(1/10,T)]
-- >                                    ,[(1/5,F),(4/5,T)]
-- >                                    ,[(1/6,F),(1/2,F),(1/3,F)]]]
--
-- > let d = [3/5,2/5,1/3,1/6,7/10,4/5,1/2,1/2]
-- > in to_divisions [(4,4)] d == Just [[[(3/5,F),(2/5,F)]
-- >                                    ,[(1/3,F),(1/6,F)]
-- >                                    ,[(2/5,T),(1/10,T)]
-- >                                    ,[(1/5,F),(4/5,F)]
-- >                                    ,[(1/2,F),(1/2,F)]]]
to_divisions :: [Time_Signature] -> [RQ] -> Maybe [[[RQ_T]]]
to_divisions ts x = to_divisions_rq (map ts_divisions ts) x

-- * Simplifications

-- > import Music.Theory.Duration.Name.Abbreviation
-- > m_simplify [(q,[Tie_Right]),(e,[Tie_Left])] == [(q',[])]
-- > m_simplify [(e,[Tie_Right]),(q,[Tie_Left])] == [(q',[])]
-- > m_simplify [(q,[Tie_Right]),(e',[Tie_Left])] == [(q'',[])]
m_simplify :: [Duration_A] -> [Duration_A]
m_simplify =
    let f (d0,a0) (d1,a1) =
            let t = Tie_Right `elem` a0 && Tie_Left `elem` a1
                e = End_Tuplet `notElem` a0
                m = duration_meq d0 d1
                d = sum_dur d0 d1
                a = delete Tie_Right a0 ++ delete Tie_Left a1
                g i = if t && e && m then Just (i,a) else Nothing
            in join (fmap g d)
    in coalesce f

-- * Durations

-- > p_tuplet_prep [(2/3,F),(1/3,T)] == Just ((3,2),[(1,F),(1/2,T)])
-- > p_tuplet_prep (map rq_rqt [1/3,1/6]) == Just ((3,2),[(1/2,F),(1/4,F)])
-- > p_tuplet_prep (map rq_rqt [2/5,1/10]) == Just ((5,4),[(1/2,F),(1/8,F)])
-- > p_tuplet_prep (map rq_rqt [1/3,1/6,2/5,1/10])
p_tuplet_prep :: [RQ_T] -> Maybe ((Integer,Integer),[RQ_T])
p_tuplet_prep x =
    let f t = (t,map (rqt_un_tuplet t) x)
    in fmap f (rq_derive_tuplet (map rqt_rq x))

-- | Within a /pulse/ tied notes that sum to a /cmn/ duration can be
-- coalesced.
--
-- > p_coalesce [(3/4,T),(1/4,F)] == [(1,F)]
-- > p_coalesce [(1/4,T),(1/8,F),(3/8,T)] == [(3/8,F),(3/8,T)]
-- > p_coalesce [(1/8,T),(1/4,T),(1/8,F)] == [(1/2,F)]
p_coalesce :: [RQ_T] -> [RQ_T]
p_coalesce =
    let f (i,p) (j,q) = if p == T && rq_is_cmn (i + j)
                        then Just (i+j,q)
                        else Nothing
    in coalesce f

-- | Notate pulse, ie. derive tuplet if neccesary.
--
-- > p_notate False [(2/3,F),(1/3,T)]
-- > p_notate False [(2/5,F),(1/10,T)]
-- > p_notate False [(1/4,T),(1/8,F),(1/8,F)]
-- > p_notate False (map rq_rqt [1/3,1/6])
-- > p_notate False (map rq_rqt [2/5,1/10])
-- > p_notate False (map rq_rqt [1/3,1/6,2/5,1/10]) == Nothing
p_notate :: Bool -> [RQ_T] -> Maybe [Duration_A]
p_notate z x =
    let f = rqt_to_duration_a z {- . p_coalesce -}
        d = case p_tuplet_prep x of
              Just (t,x') -> da_tuplet t (f x')
              Nothing -> f x
    in if rq_can_notate (map rqt_rq x) then Just d else Nothing

-- | Notate measure.
--
-- > m_notate True [[(2/3,F),(1/3,T)],[(1,T)],[(1,F)]]
m_notate :: Bool -> [[RQ_T]] -> [Duration_A]
m_notate z m =
    let z' = z : map (is_tied_right . last) m
    in concat (catMaybes (zipWith p_notate z' m))

-- > let d = [2/7,1/7,4/7,5/7,8/7,1,1/7]
-- > in fmap notate (to_divisions [[1,1,1,1]] d)
--
-- > let d = [2/7,1/7,4/7,5/7,1,6/7,3/7]
-- > in fmap notate (to_divisions [[1,1,1,1]] d)
--
-- > let d = [3/5,2/5,1/3,1/6,7/10,4/5,1/2,1/2]
-- > in fmap notate (to_divisions [[1,1/2,1/2,1,1]] d)
--
-- > m_notate False [map rq_rqt [3/5,2/5,1/3,1/6,7/10,17/15,1/2,1/6]]
-- > m_notate False [map rq_rqt [3/5,2/5,1/3,1/6,7/10,29/30,1/2,1/3]]
notate :: [[[RQ_T]]] -> [Duration_A]
notate d =
    let z = False : map (is_tied_right . last . last) d
    in concat (zipWith m_notate z d)

-- * Ascribe

-- | Variant of 'zip' that retains elements of the right hand list
-- where elements of the left hand list meet the given predicate.
--
-- > zip_hold even [1..5] "abc" == [(1,'a'),(2,'b'),(3,'b'),(4,'c'),(5,'c')]
-- > zip_hold odd [1..5] "abc" == [(1,'a'),(2,'a'),(3,'b'),(4,'b'),(5,'c')]
zip_hold :: (x -> Bool) -> [x] -> [a] -> [(x,a)]
zip_hold fn =
    let go [] _ = []
        go _ [] = error "zip_hold"
        go (x:xs) (i:is) = let is' = if fn x then i:is else is
                           in (x,i) : go xs is'
    in go

-- | Zip a list of 'Duration_A' elements duplicating elements of the
-- right hand sequence for tied durations.
--
-- > map snd (ascribe (notate' [4,4] [3,3,2]) "xyz") == "xyyz"
ascribe :: [Duration_A] -> [x] -> [(Duration_A,x)]
ascribe = zip_hold da_tied_right
