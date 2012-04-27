-- | Notation of a sequence of 'RQ' values as annotated 'Duration' values.
--
-- 1. Separate input sequence into measures, adding tie annotations as
-- required (see 'to_measures_ts').  Ensure all 'RQ_T' values can be
-- notated as /common music notation/ durations.
--
-- 2. Separate each measure into pulses (see 'm_divisions_ts').
-- Further subdivides pulses to ensure /cmn/ tuplet notation.  See
-- 'to_divisions_ts' for a composition of 'to_measures_ts' and
-- 'm_divisions_ts'.
--
-- 3. Simplify each measure (see 'm_simplify' and 'default_rule').
-- Coalesces tied durations where appropriate.
--
-- 4. Notate measures (see 'm_notate' or 'mm_notate').
--
-- 5. Ascribe values to notated durations, see 'ascribe'.
module Music.Theory.Duration.Sequence.Notate where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Ratio
import Music.Theory.Duration
import Music.Theory.Duration.Annotation
import Music.Theory.Duration.RQ
import Music.Theory.Duration.RQ.Tied
import Music.Theory.Time_Signature

-- * Lists

-- | Variant of 'catMaybes'.  If all elements of the list are @Just
-- a@, then gives @Just [a]@ else gives 'Nothing'.
--
-- > all_just (map Just [1..3]) == Just [1..3]
-- > all_just [Just 1,Nothing,Just 3] == Nothing
all_just :: [Maybe a] -> Maybe [a]
all_just x =
    case x of
      [] -> Just []
      Just i:x' -> fmap (i :) (all_just x')
      Nothing:_ -> Nothing

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

-- | Variant of 'coalesce' with accumulation parameter.
--
-- > coalesce_accum (\i p q -> Left (p + q)) 0 [1..5] == [(0,15)]
--
-- > let jn i p q = if even p then Left (p + q) else Right (p + i)
-- > in coalesce_accum jn 0 [1..7] == [(0,1),(1,5),(6,9),(15,13)]
--
-- > let jn i p q = if even p then Left (p + q) else Right [p,q]
-- > in coalesce_accum jn [] [1..5] == [([],1),([1,2],5),([5,4],9)]
coalesce_accum :: (b -> a -> a -> Either a b) -> b -> [a] -> [(b,a)]
coalesce_accum f i x =
    case x of
      [] -> []
      [p] -> [(i,p)]
      (p:q:x') ->
          case f i p q of
            Right j -> (i,p) : coalesce_accum f j (q : x')
            Left r -> coalesce_accum f i (r : x')

-- | Variant of 'coalesce_accum' that accumulates running sum.
--
-- > let f i p q = if i == 1 then Just (p + q) else Nothing
-- > in coalesce_sum (+) 0 f [1,1/2,1/4,1/4] == [1,1]
coalesce_sum :: (b -> a -> b) -> b -> (b -> a -> a -> Maybe a) -> [a] -> [a]
coalesce_sum add zero f =
    let g i p q = case f i p q of
                    Just r -> Left r
                    Nothing -> Right (i `add` p)
    in map snd . coalesce_accum g zero

-- * Separate

-- | Take elements while the sum of the prefix is less than or equal
-- to the indicated value.  Returns also the difference between the
-- prefix sum and the requested sum.  Note that zero elements are kept
-- left.
--
-- > take_sum_by id 3 [2,1] == ([2,1],0,[])
-- > take_sum_by id 3 [2,2] == ([2],1,[2])
-- > take_sum_by id 3 [2,1,0,1] == ([2,1,0],0,[1])
-- > take_sum_by id 3 [4] == ([],3,[4])
-- > take_sum_by id 0 [1..5] == ([],0,[1..5])
take_sum_by :: (Ord n, Num n) => (a -> n) -> n -> [a] -> ([a], n, [a])
take_sum_by f m =
    let go r n l =
            let z = (reverse r,m-n,l)
            in case l of
                 [] -> z
                 i:l' -> let n' = f i + n
                         in if n' > m
                            then z
                            else go (i:r) n' l'
    in go [] 0

-- | Variant of 'take_sum_by' with 'id' function.
take_sum :: (Ord a, Num a) => a -> [a] -> ([a],a,[a])
take_sum = take_sum_by id

-- | Variant of 'take_sum' that requires the prefix to sum to value.
--
-- > take_sum_by_eq id 3 [2,1,0,1] == Just ([2,1,0],[1])
-- > take_sum_by_eq id 3 [2,2] == Nothing
take_sum_by_eq :: (Ord n, Num n) => (a -> n) -> n -> [a] -> Maybe ([a], [a])
take_sum_by_eq f m l =
    case take_sum_by f m l of
      (p,0,q) -> Just (p,q)
      _ -> Nothing

-- | Recursive variant of 'take_sum_by_eq'.
--
-- > split_sum_by_eq id [3,3] [2,1,0,3] == Just [[2,1,0],[3]]
-- > split_sum_by_eq id [3,3] [2,2,2] == Nothing
split_sum_by_eq :: (Ord n, Num n) => (a -> n) -> [n] -> [a] -> Maybe [[a]]
split_sum_by_eq f mm l =
    case (mm,l) of
      ([],[]) -> Just []
      (m:mm',_) -> case take_sum_by_eq f m l of
                     Just (p,l') -> fmap (p :) (split_sum_by_eq f mm' l')
                     Nothing -> Nothing
      _ -> Nothing

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

-- | Alias for 'True', used locally for documentation.
_t :: Bool
_t = True

-- | Alias for 'False', used locally for documentation.
_f :: Bool
_f = False

-- | Variant of 'split_sum' that operates at 'RQ_T' sequences.
--
-- > let r = Just ([(3,_f),(2,_t)],[(1,_f)])
-- > in rqt_split_sum 5 [(3,_f),(2,_t),(1,_f)] == r
--
-- > let r = Just ([(3,_f),(1,_t)],[(1,_t),(1,_f)])
-- > in rqt_split_sum 4 [(3,_f),(2,_t),(1,_f)] == r
rqt_split_sum :: RQ -> [RQ_T] -> Maybe ([RQ_T],[RQ_T])
rqt_split_sum d x =
    case split_sum d (map rqt_rq x) of
      Just (i,_,k) ->
          case k of
            Nothing -> Just (splitAt (length i) x)
            Just (p,q) -> let (s,(_,z):t) = splitAt (length i - 1) x
                          in Just (s ++ [(p,True)]
                                  ,((q,z) : t))
      Nothing -> Nothing

-- | Separate 'RQ_T' values in sequences summing to 'RQ' values.  This
-- is a recursive variant of 'rqt_split_sum'.  Note that is does not
-- ensure /cmn/ notation of values.
--
-- > let d = [(2,_f),(2,_f),(2,_f)]
-- > in rqt_separate [3,3] d == Just [[(2,_f),(1,_t)]
-- >                                 ,[(1,_f),(2,_f)]]
--
-- > let d = [(5/8,_f),(1,_f),(3/8,_f)]
-- > in rqt_separate [1,1] d == Just [[(5/8,_f),(3/8,_t)],[(5/8,_f),(3/8,_f)]]
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
-- > rqt_separate_tuplet undefined [(1/3,_f),(1/6,_f)] == Nothing
-- > rqt_separate_tuplet undefined [(4/7,_t),(1/7,_f),(2/7,_f)] == Nothing
--
-- > let d = map rq_rqt [1/3,1/6,2/5,1/10]
-- > in rqt_separate_tuplet (1/8) d == Just [[(1/3,_f),(1/6,_f)]
-- >                                        ,[(2/5,_f),(1/10,_f)]]
rqt_separate_tuplet :: RQ -> [RQ_T] -> Maybe [[RQ_T]]
rqt_separate_tuplet i x =
    if rqt_can_notate x
    then Nothing
    else let j = sum (map rqt_rq x) / 2
         in if j < i then Nothing else rqt_separate [j,j] x

-- | Recursive variant of 'rqt_separate_tuplet'.
--
-- > let d = map rq_rqt [1,1/3,1/6,2/5,1/10]
-- > in rqt_tuplet_subdivide (1/8) d == [[(1/1,_f)]
-- >                                    ,[(1/3,_f),(1/6,_f)]
-- >                                    ,[(2/5,_f),(1/10,_f)]]
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
-- > to_measures_rq [3,3] [2,2,2] == Just [[(2,_f),(1,_t)],[(1,_f),(2,_f)]]
-- > to_measures_rq [3,3] [6] == Just [[(3,_t)],[(3,_f)]]
-- > to_measures_rq [1,1,1] [3] == Just [[(1,_t)],[(1,_t)],[(1,_f)]]
-- > to_measures_rq [3,3] [2,2,1] == Nothing
-- > to_measures_rq [3,2] [2,2,2] == Nothing
-- > to_measures_rq [6,6] [5,5,2] == Just [[(4,_t),(1,_f),(1,_t)]
-- >                                      ,[(4,_f),(2,_f)]]
to_measures_rq :: [RQ] -> [RQ] -> Maybe [[RQ_T]]
to_measures_rq m = fmap (map rqt_set_to_cmn) . rqt_separate m . map rq_rqt

-- | Variant of 'to_measures_rq' with measures given by
-- 'Time_Signature' values.  Ensures 'RQ_T' are /cmn/ durations.
--
-- > to_measures_ts [(1,4)] [5/8,3/8] == Just [[(1/2,_t),(1/8,_f),(3/8,_f)]]
-- > to_measures_ts [(1,4)] [5/7,2/7] == Just [[(4/7,_t),(1/7,_f),(2/7,_f)]]
--
-- > let {m = replicate 18 (1,4)
-- >     ;x = [3/4,2,5/4,9/4,1/4,3/2,1/2,7/4,1,5/2,11/4,3/2]}
-- > in to_measures_ts m x == Just [[(3/4,_f),(1/4,_t)],[(1/1,_t)]
-- >                              ,[(3/4,_f),(1/4,_t)],[(1/1,_f)]
-- >                              ,[(1/1,_t)],[(1/1,_t)]
-- >                              ,[(1/4,_f),(1/4,_f),(1/2,_t)],[(1/1,_f)]
-- >                              ,[(1/2,_f),(1/2,_t)],[(1/1,_t)]
-- >                              ,[(1/4,_f),(3/4,_t)],[(1/4,_f),(3/4,_t)]
-- >                              ,[(1/1,_t)],[(3/4,_f),(1/4,_t)]
-- >                              ,[(1/1,_t)],[(1/1,_t)]
-- >                              ,[(1/2,_f),(1/2,_t)],[(1/1,_f)]]
to_measures_ts :: [Time_Signature] -> [RQ] -> Maybe [[RQ_T]]
to_measures_ts m = to_measures_rq (map ts_rq m)

-- | Variant of 'to_measures_ts' that allows for duration field
-- operation but requires that measures be well formed.  This is
-- useful for re-grouping measures after notation and ascription.
to_measures_ts_by_eq :: (a -> RQ) -> [Time_Signature] -> [a] -> Maybe [[a]]
to_measures_ts_by_eq f m = split_sum_by_eq f (map ts_rq m)

-- | Divide measure into pulses of indicated 'RQ' durations.  Measure
-- must be of correct length and contain only /cmn/ durations.  Pulses
-- are further subdivided if required to notate tuplets correctly, see
-- 'rqt_tuplet_subdivide_seq'.
m_divisions_rq :: [RQ] -> [RQ_T] -> Maybe [[RQ_T]]
m_divisions_rq z = fmap (rqt_tuplet_subdivide_seq (1/16)) . rqt_separate z

-- | Variant of 'm_divisions_rq' that determines pulse divisions from
-- 'Time_Signature'.
--
-- > let d = [(4/7,_t),(1/7,_f),(2/7,_f)]
-- > in m_divisions_ts (1,4) d == Just [d]
--
-- > let d = map rq_rqt [1/3,1/6,2/5,1/10]
-- > in m_divisions_ts (1,4) d == Just [[(1/3,_f),(1/6,_f)]
-- >                                   ,[(2/5,_f),(1/10,_f)]]
m_divisions_ts :: Time_Signature -> [RQ_T] -> Maybe [[RQ_T]]
m_divisions_ts ts = m_divisions_rq (ts_divisions ts)

-- | Composition of 'to_measures_rq' and 'm_divisions_rq', where
-- measures are initially given as sets of divisions.
--
-- > let m = [[1,1,1],[1,1,1]]
-- > in to_divisions_rq m [2,2,2] == Just [[[(1,_t)],[(1,_f)],[(1,_t)]]
-- >                                      ,[[(1,_f)],[(1,_t)],[(1,_f)]]]
--
-- > let d = [2/7,1/7,4/7,5/7,8/7,1,1/7]
-- > in to_divisions_rq [[1,1,1,1]] d == Just [[[(2/7,_f),(1/7,_f),(4/7,_f)]
-- >                                           ,[(4/7,_t),(1/7,_f),(2/7,_t)]
-- >                                           ,[(6/7,_f),(1/7,_t)]
-- >                                           ,[(6/7,_f),(1/7,_f)]]]
--
-- > let d = [2/7,1/7,4/7,5/7,1,6/7,3/7]
-- > in to_divisions_rq [[1,1,1,1]] d == Just [[[(2/7,_f),(1/7,_f),(4/7,_f)]
-- >                                           ,[(4/7,_t),(1/7,_f),(2/7,_t)]
-- >                                           ,[(1/2,_t)]
-- >                                           ,[(3/14,_f),(2/7,_t)]
-- >                                           ,[(4/7,_f),(3/7,_f)]]]
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
-- > in to_divisions_ts [(4,4)] d == Just [[[(3/5,_f),(2/5,_f)]
-- >                                      ,[(1/3,_f),(1/6,_f)]
-- >                                      ,[(2/5,_t),(1/10,_t)]
-- >                                      ,[(1/5,_f),(4/5,_t)]
-- >                                      ,[(1/3,_f),(1/2,_f),(1/6,_f)]]]
--
-- > let d = [3/5,2/5,1/3,1/6,7/10,29/30,1/2,1/3]
-- > in to_divisions_ts [(4,4)] d == Just [[[(3/5,_f),(2/5,_f)]
-- >                                      ,[(1/3,_f),(1/6,_f)]
-- >                                      ,[(2/5,_t),(1/10,_t)]
-- >                                      ,[(1/5,_f),(4/5,_t)]
-- >                                      ,[(1/6,_f),(1/2,_f),(1/3,_f)]]]
--
-- > let d = [3/5,2/5,1/3,1/6,7/10,4/5,1/2,1/2]
-- > in to_divisions_ts [(4,4)] d == Just [[[(3/5,_f),(2/5,_f)]
-- >                                      ,[(1/3,_f),(1/6,_f)]
-- >                                      ,[(2/5,_t),(1/10,_t)]
-- >                                      ,[(1/5,_f),(4/5,_f)]
-- >                                      ,[(1/2,_f),(1/2,_f)]]]
to_divisions_ts :: [Time_Signature] -> [RQ] -> Maybe [[[RQ_T]]]
to_divisions_ts ts x = to_divisions_rq (map ts_divisions ts) x

-- * Durations

-- | Pulse tuplet derivation.
--
-- > p_tuplet_rqt [(2/3,_f),(1/3,_t)] == Just ((3,2),[(1,_f),(1/2,_t)])
-- > p_tuplet_rqt (map rq_rqt [1/3,1/6]) == Just ((3,2),[(1/2,_f),(1/4,_f)])
-- > p_tuplet_rqt (map rq_rqt [2/5,1/10]) == Just ((5,4),[(1/2,_f),(1/8,_f)])
-- > p_tuplet_rqt (map rq_rqt [1/3,1/6,2/5,1/10])
p_tuplet_rqt :: [RQ_T] -> Maybe ((Integer,Integer),[RQ_T])
p_tuplet_rqt x =
    let f t = (t,map (rqt_un_tuplet t) x)
    in fmap f (rq_derive_tuplet (map rqt_rq x))

-- | Notate pulse, ie. derive tuplet if neccesary. The flag indicates
-- if the initial value is tied left.
--
-- > p_notate False [(2/3,_f),(1/3,_t)]
-- > p_notate False [(2/5,_f),(1/10,_t)]
-- > p_notate False [(1/4,_t),(1/8,_f),(1/8,_f)]
-- > p_notate False (map rq_rqt [1/3,1/6])
-- > p_notate False (map rq_rqt [2/5,1/10])
-- > p_notate False (map rq_rqt [1/3,1/6,2/5,1/10]) == Nothing
p_notate :: Bool -> [RQ_T] -> Maybe [Duration_A]
p_notate z x =
    let f = p_simplify . rqt_to_duration_a z
        d = case p_tuplet_rqt x of
              Just (t,x') -> da_tuplet t (f x')
              Nothing -> f x
    in if rq_can_notate (map rqt_rq x) then Just d else Nothing

-- | Notate measure.
--
-- > m_notate True [[(2/3,_f),(1/3,_t)],[(1,_t)],[(1,_f)]]
--
-- > m_notate False [map rq_rqt [3/5,2/5,1/3,1/6,7/10,17/15,1/2,1/6]]
-- > m_notate False [map rq_rqt [3/5,2/5,1/3,1/6,7/10,29/30,1/2,1/3]]
m_notate :: Bool -> [[RQ_T]] -> Maybe [Duration_A]
m_notate z m =
    let z' = z : map (is_tied_right . last) m
    in fmap concat (all_just (zipWith p_notate z' m))

-- | Multiple measure notation.
--
-- > let d = [2/7,1/7,4/7,5/7,8/7,1,1/7]
-- > in fmap mm_notate (to_divisions_ts [(4,4)] d)
--
-- > let d = [2/7,1/7,4/7,5/7,1,6/7,3/7]
-- > in fmap mm_notate (to_divisions_ts [(4,4)] d)
--
-- > let d = [3/5,2/5,1/3,1/6,7/10,4/5,1/2,1/2]
-- > in fmap mm_notate (to_divisions_ts [(4,4)] d)
mm_notate :: [[[RQ_T]]] -> Maybe [[Duration_A]]
mm_notate d =
    let z = False : map (is_tied_right . last . last) d
    in all_just (zipWith m_notate z d)

-- * Simplifications

-- | Structure given to 'Simplify_P' to decide simplification.  The
-- structure is /(ts,start-rq,(left-rq,right-rq))/.
type Simplify_T = (Time_Signature,RQ,(RQ,RQ))

-- | Predicate function at 'Simplify_T'.
type Simplify_P = Simplify_T -> Bool

-- | Variant of 'Simplify_T' allowing multiple rules.
type Simplify_M = ([Time_Signature],[RQ],[(RQ,RQ)])

-- | Transform 'Simplify_M' to 'Simplify_P'.
meta_table_p :: Simplify_M -> Simplify_P
meta_table_p (tt,ss,pp) (t,s,p) = t `elem` tt && s `elem` ss && p `elem` pp

-- | Transform 'Simplify_M' to set of 'Simplify_T'.
meta_table_t :: Simplify_M -> [Simplify_T]
meta_table_t (tt,ss,pp) = [(t,s,p) | t <- tt, s <- ss,p <- pp]

-- | The default table of simplifiers.
--
-- > default_table ((3,4),1,(1,1)) == True
default_table :: Simplify_P
default_table x =
    let t :: [Simplify_M]
        t = [([(3,4)],[1],[(1,1)])]
        p :: [Simplify_P]
        p = map meta_table_p t
    in or (p <*> pure x)

-- | The default eighth-note pulse simplifier rule.
--
-- > default_e_rule ((3,8),0,(1/2,1/2)) == True
-- > default_e_rule ((3,8),1/2,(1/2,1/2)) == True
-- > default_e_rule ((3,8),1,(1/2,1/2)) == True
-- > default_e_rule ((2,8),0,(1/2,1/2)) == True
-- > default_e_rule ((5,8),0,(1,1/2)) == True
-- > default_e_rule ((5,8),0,(2,1/2)) == True
default_e_rule :: Simplify_P
default_e_rule ((i,j),t,(p,q)) =
    let r = p + q
    in j == 8 &&
       denominator t `elem` [1,2] &&
       (r <= 2 || r == ts_rq (i,j) || rq_is_integral r)

-- | The default quarter note pulse simplifier rule.
--
-- > default_q_rule ((3,4),0,(1,1/2)) == True
-- > default_q_rule ((3,4),0,(1,3/4)) == True
-- > default_q_rule ((4,4),1,(1,1)) == False
-- > default_q_rule ((4,4),2,(1,1)) == True
-- > default_q_rule ((4,4),2,(1,2)) == True
-- > default_q_rule ((4,4),0,(2,1)) == True
-- > default_q_rule ((3,4),1,(1,1)) == False
default_q_rule :: Simplify_P
default_q_rule ((_,j),t,(p,q)) =
    let r = p + q
    in j == 4 &&
       denominator t == 1 &&
       even (numerator t) &&
       (r <= 2 || rq_is_integral r)

-- | The default simplifier rule.  To extend provide a list of
-- 'Simplify_T'.
default_rule :: [Simplify_T] -> Simplify_P
default_rule x r = r `elem` x ||
                   default_q_rule r ||
                   default_e_rule r ||
                   default_table r

-- | Measure simplifier.  Apply given 'Simplify_P'.
m_simplify :: Simplify_P -> Time_Signature -> [Duration_A] -> [Duration_A]
m_simplify p ts =
    let f st (d0,a0) (d1,a1) =
            let t = Tie_Right `elem` a0 && Tie_Left `elem` a1
                e = End_Tuplet `notElem` a0 || any begins_tuplet a1
                m = duration_meq d0 d1
                d = sum_dur d0 d1
                a = delete Tie_Right a0 ++ delete Tie_Left a1
                r = p (ts,st,(duration_to_rq d0,duration_to_rq d1))
                n_dots = 1
                g i = if dots i <= n_dots && t && e && m && r
                      then Just (i,a)
                      else Nothing
            in join (fmap g d)
        z i (j,_) = i + duration_to_rq j
    in coalesce_sum z 0 f

-- | Pulse simplifier predicate, which is 'const' 'True'.
p_simplify_rule :: Simplify_P
p_simplify_rule = const True

-- | Pulse simplifier.
--
-- > import Music.Theory.Duration.Name.Abbreviation
-- > p_simplify [(q,[Tie_Right]),(e,[Tie_Left])] == [(q',[])]
-- > p_simplify [(e,[Tie_Right]),(q,[Tie_Left])] == [(q',[])]
-- > p_simplify [(q,[Tie_Right]),(e',[Tie_Left])] == [(q'',[])]
-- > p_simplify [(q'',[Tie_Right]),(s,[Tie_Left])] == [(h,[])]
-- > p_simplify [(e,[Tie_Right]),(s,[Tie_Left]),(e',[])] == [(e',[]),(e',[])]
--
-- > let f = rqt_to_duration_a False
-- > in p_simplify (f [(1/8,_t),(1/4,_t),(1/8,_f)]) == f [(1/2,_f)]
p_simplify :: [Duration_A] -> [Duration_A]
p_simplify = m_simplify p_simplify_rule undefined

-- * Notate

-- | Composition of 'to_divisions_ts', 'mm_notate' 'm_simplify'.
notate :: Simplify_P -> [Time_Signature] -> [RQ] -> Maybe [[Duration_A]]
notate r ts x = do
    mm <- to_divisions_ts ts x
    dd <- mm_notate mm
    return (zipWith (m_simplify r) ts dd)

-- * Ascribe

-- | Variant of 'zip' that retains elements of the right hand list
-- where elements of the left hand list meet the given predicate.  If
-- the right hand side is longer the remaining elements to be
-- processed are given.  It is an error for the right hand side to be
-- short.
--
-- > zip_hold even [1..5] "abc" == ([],zip [1..6] "abbcc")
-- > zip_hold odd [1..6] "abc" == ([],zip [1..6] "aabbcc")
-- > zip_hold even [1] "ab" == ("b",[(1,'a')])
-- > zip_hold even [1,2] "a" == undefined
zip_hold :: (x -> Bool) -> [x] -> [t] -> ([t],[(x,t)])
zip_hold fn =
    let f st e =
            case st of
              r:s -> let st' = if fn e then st else s
                     in (st',(e,r))
              _ -> error "zip_hold: rhs ends"
    in flip (mapAccumL f)

-- | Variant of 'zip_hold' that requires the right hand side to be
-- precisely the required length.
--
-- > zip_hold_err even [1..5] "abc" == zip [1..6] "abbcc"
-- > zip_hold_err odd [1..6] "abc" == zip [1..6] "aabbcc"
-- > zip_hold_err id [False,False] "a" == undefined
-- > zip_hold_err id [False] "ab" == undefined
zip_hold_err :: (x -> Bool) -> [x] -> [a] -> [(x,a)]
zip_hold_err fn p q =
    case zip_hold fn p q of
      ([],r) -> r
      _ -> error "zip_hold_err: lhs ends"

-- | Zip a list of 'Duration_A' elements duplicating elements of the
-- right hand sequence for tied durations.
--
-- > let {Just d = to_divisions_ts [(4,4),(4,4)] [3,3,2]
-- >     ;f = map snd . snd . flip m_ascribe "xyz"}
-- > in fmap f (notate d) == Just "xxxyyyzz"
m_ascribe :: [Duration_A] -> [x] -> ([x],[(Duration_A,x)])
m_ascribe = zip_hold da_tied_right

-- | Variant of 'm_ascribe' for a set of measures.
mm_ascribe :: [[Duration_A]] -> [x] -> [[(Duration_A,x)]]
mm_ascribe mm x =
    case mm of
      [] -> []
      m:mm' -> let (x',r) = m_ascribe m x
               in r : mm_ascribe mm' x'

-- | 'snd' '.' 'm_ascribe'.
ascribe :: [Duration_A] -> [x] -> [(Duration_A, x)]
ascribe d = snd . m_ascribe d
