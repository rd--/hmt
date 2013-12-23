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

import Control.Applicative {- base -}
import Control.Monad {- base -}
import Data.List {- base -}
import Data.List.Split {- split -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}

import Music.Theory.Duration {- hmt -}
import Music.Theory.Duration.Annotation {- hmt -}
import Music.Theory.Function {- hmt -}
import Music.Theory.Duration.RQ {- hmt -}
import Music.Theory.Duration.RQ.Tied {- hmt -}
import Music.Theory.List {- hmt -}
import Music.Theory.Time_Signature {- hmt -}

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

-- | Variant of 'Data.Either.rights' that preserves first 'Left'.
--
-- > all_right (map Right [1..3]) == Right [1..3]
-- > all_right [Right 1,Left 'a',Left 'b'] == Left 'a'
all_right :: [Either a b] -> Either a [b]
all_right x =
    case x of
      [] -> Right []
      Right i:x' -> fmap (i :) (all_right x')
      Left i:_ -> Left i

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

-- * Either

-- | Lower 'Either' to 'Maybe' by discarding 'Left'.
either_to_maybe :: Either a b -> Maybe b
either_to_maybe = either (const Nothing) Just

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
-- an element.  Note that zero elements are kept left.  If the required
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
--
-- > rqt_split_sum 4 [(5/2,False)] == Nothing
rqt_split_sum :: RQ -> [RQ_T] -> Maybe ([RQ_T],[RQ_T])
rqt_split_sum d x =
    case split_sum d (map rqt_rq x) of
      Just (i,_,k) ->
          case k of
            Nothing -> Just (splitAt (length i) x)
            Just (p,q) -> let (s,(_,z):t) = splitAt (length i - 1) x
                          in Just (s ++ [(p,True)]
                                  ,(q,z) : t)
      Nothing -> Nothing

-- | Separate 'RQ_T' values in sequences summing to 'RQ' values.  This
-- is a recursive variant of 'rqt_split_sum'.  Note that is does not
-- ensure /cmn/ notation of values.
--
-- > let d = [(2,_f),(2,_f),(2,_f)]
-- > in rqt_separate [3,3] d == Right [[(2,_f),(1,_t)]
-- >                                  ,[(1,_f),(2,_f)]]
--
-- > let d = [(5/8,_f),(1,_f),(3/8,_f)]
-- > in rqt_separate [1,1] d == Right [[(5/8,_f),(3/8,_t)]
-- >                                  ,[(5/8,_f),(3/8,_f)]]
--
-- > let d = [(4/7,_t),(1/7,_f),(1,_f),(6/7,_f),(3/7,_f)]
-- > in rqt_separate [1,1,1] d == Right [[(4/7,_t),(1/7,_f),(2/7,_t)]
-- >                                    ,[(5/7,_f),(2/7,_t)]
-- >                                    ,[(4/7,_f),(3/7,_f)]]
rqt_separate :: [RQ] -> [RQ_T] -> Either String [[RQ_T]]
rqt_separate m x =
    case (m,x) of
      ([],[]) -> Right []
      ([],_) -> Left (show ("rqt_separate",x))
      (i:m',_) ->
          case rqt_split_sum i x of
            Just (r,x') -> fmap (r :) (rqt_separate m' x')
            Nothing -> Left (show ("rqt_separate",i,m',x))

rqt_separate_m :: [RQ] -> [RQ_T] -> Maybe [[RQ_T]]
rqt_separate_m m = either_to_maybe . rqt_separate m

-- | If the input 'RQ_T' sequence cannot be notated (see
-- 'rqt_can_notate') separate into equal parts, so long as each part
-- is not less than /i/.
--
-- > rqt_separate_tuplet undefined [(1/3,_f),(1/6,_f)]
-- > rqt_separate_tuplet undefined [(4/7,_t),(1/7,_f),(2/7,_f)]
--
-- > let d = map rq_rqt [1/3,1/6,2/5,1/10]
-- > in rqt_separate_tuplet (1/8) d == Right [[(1/3,_f),(1/6,_f)]
-- >                                         ,[(2/5,_f),(1/10,_f)]]
--
-- > let d = [(1/5,True),(1/20,False),(1/2,False),(1/4,True)]
-- > in rqt_separate_tuplet (1/16) d
--
-- > let d = [(2/5,_f),(1/5,_f),(1/5,_f),(1/5,_t),(1/2,_f),(1/2,_f)]
-- > in rqt_separate_tuplet (1/2) d
--
-- > let d = [(4/10,True),(1/10,False),(1/2,True)]
-- > in rqt_separate_tuplet (1/2) d
rqt_separate_tuplet :: RQ -> [RQ_T] -> Either String [[RQ_T]]
rqt_separate_tuplet i x =
    if rqt_can_notate x
    then Left (show ("rqt_separate_tuplet: separation not required",x))
    else let j = sum (map rqt_rq x) / 2
         in if j < i
            then Left (show ("rqt_separate_tuplet: j < i",j,i))
            else rqt_separate [j,j] x

-- | Recursive variant of 'rqt_separate_tuplet'.
--
-- > let d = map rq_rqt [1,1/3,1/6,2/5,1/10]
-- > in rqt_tuplet_subdivide (1/8) d == [[(1/1,_f)]
-- >                                    ,[(1/3,_f),(1/6,_f)]
-- >                                    ,[(2/5,_f),(1/10,_f)]]
rqt_tuplet_subdivide :: RQ -> [RQ_T] -> [[RQ_T]]
rqt_tuplet_subdivide i x =
    case rqt_separate_tuplet i x of
      Left _ -> [x]
      Right r -> concatMap (rqt_tuplet_subdivide i) r

-- | Sequence variant of 'rqt_tuplet_subdivide'.
--
-- > let d = [(1/5,True),(1/20,False),(1/2,False),(1/4,True)]
-- > in rqt_tuplet_subdivide_seq (1/2) [d]
rqt_tuplet_subdivide_seq :: RQ -> [[RQ_T]] -> [[RQ_T]]
rqt_tuplet_subdivide_seq i = concatMap (rqt_tuplet_subdivide i)

-- | If a tuplet is all tied, it ought to be a plain value?!
--
-- > rqt_tuplet_sanity_ [(4/10,_t),(1/10,_f)] == [(1/2,_f)]
rqt_tuplet_sanity_ :: [RQ_T] -> [RQ_T]
rqt_tuplet_sanity_ t =
    let last_tied = rqt_tied (last t)
        all_tied = all rqt_tied (dropRight 1 t)
    in if all_tied
       then [(sum (map rqt_rq t),last_tied)]
       else t

rqt_tuplet_subdivide_seq_sanity_ :: RQ -> [[RQ_T]] -> [[RQ_T]]
rqt_tuplet_subdivide_seq_sanity_ i =
    map rqt_tuplet_sanity_ .
    rqt_tuplet_subdivide_seq i

-- * Divisions

-- | Separate 'RQ' sequence into measures given by 'RQ' length.
--
-- > to_measures_rq [3,3] [2,2,2] == Right [[(2,_f),(1,_t)],[(1,_f),(2,_f)]]
-- > to_measures_rq [3,3] [6] == Right [[(3,_t)],[(3,_f)]]
-- > to_measures_rq [1,1,1] [3] == Right [[(1,_t)],[(1,_t)],[(1,_f)]]
-- > to_measures_rq [3,3] [2,2,1]
-- > to_measures_rq [3,2] [2,2,2]
--
-- > let d = [4/7,33/28,9/20,4/5]
-- > in to_measures_rq [3] d == Right [[(4/7,_f),(33/28,_f),(9/20,_f),(4/5,_f)]]
to_measures_rq :: [RQ] -> [RQ] -> Either String [[RQ_T]]
to_measures_rq m = rqt_separate m . map rq_rqt

-- | Variant of 'to_measures_rq' that ensures 'RQ_T' are /cmn/
-- durations.  This is not a good composition.
--
-- > to_measures_rq_cmn [6,6] [5,5,2] == Right [[(4,_t),(1,_f),(1,_t)]
-- >                                           ,[(4,_f),(2,_f)]]
--
-- > let r = [[(4/7,_t),(1/7,_f),(1,_f),(6/7,_f),(3/7,_f)]]
-- > in to_measures_rq_cmn [3] [5/7,1,6/7,3/7] == Right r
--
-- > to_measures_rq_cmn [1,1,1] [5/7,1,6/7,3/7] == Right [[(4/7,_t),(1/7,_f),(2/7,_t)]
-- >                                                     ,[(4/7,_t),(1/7,_f),(2/7,_t)]
-- >                                                     ,[(4/7,_f),(3/7,_f)]]
to_measures_rq_cmn :: [RQ] -> [RQ] -> Either String [[RQ_T]]
to_measures_rq_cmn m = fmap (map rqt_set_to_cmn) . to_measures_rq m

-- | Variant of 'to_measures_rq' with measures given by
-- 'Time_Signature' values.  Does not ensure 'RQ_T' are /cmn/
-- durations.
--
-- > to_measures_ts [(1,4)] [5/8,3/8] /= Right [[(1/2,_t),(1/8,_f),(3/8,_f)]]
-- > to_measures_ts [(1,4)] [5/7,2/7] /= Right [[(4/7,_t),(1/7,_f),(2/7,_f)]]
--
-- > let {m = replicate 18 (1,4)
-- >     ;x = [3/4,2,5/4,9/4,1/4,3/2,1/2,7/4,1,5/2,11/4,3/2]}
-- > in to_measures_ts m x == Right [[(3/4,_f),(1/4,_t)],[(1/1,_t)]
-- >                                ,[(3/4,_f),(1/4,_t)],[(1/1,_f)]
-- >                                ,[(1/1,_t)],[(1/1,_t)]
-- >                                ,[(1/4,_f),(1/4,_f),(1/2,_t)],[(1/1,_f)]
-- >                                ,[(1/2,_f),(1/2,_t)],[(1/1,_t)]
-- >                                ,[(1/4,_f),(3/4,_t)],[(1/4,_f),(3/4,_t)]
-- >                                ,[(1/1,_t)],[(3/4,_f),(1/4,_t)]
-- >                                ,[(1/1,_t)],[(1/1,_t)]
-- >                                ,[(1/2,_f),(1/2,_t)],[(1/1,_f)]]
--
-- > to_measures_ts [(3,4)] [4/7,33/28,9/20,4/5]
-- > to_measures_ts (replicate 3 (1,4)) [4/7,33/28,9/20,4/5]
to_measures_ts :: [Time_Signature] -> [RQ] -> Either String [[RQ_T]]
to_measures_ts m = to_measures_rq (map ts_rq m)

-- | Variant of 'to_measures_ts' that allows for duration field
-- operation but requires that measures be well formed.  This is
-- useful for re-grouping measures after notation and ascription.
to_measures_ts_by_eq :: (a -> RQ) -> [Time_Signature] -> [a] -> Maybe [[a]]
to_measures_ts_by_eq f m = split_sum_by_eq f (map ts_rq m)

-- | Divide measure into pulses of indicated 'RQ' durations.  Measure
-- must be of correct length but need not contain only /cmn/
-- durations.  Pulses are further subdivided if required to notate
-- tuplets correctly, see 'rqt_tuplet_subdivide_seq'.
--
-- > let d = [(1/4,_f),(1/4,_f),(2/3,_t),(1/6,_f),(16/15,_f),(1/5,_f)
-- >         ,(1/5,_f),(2/5,_t),(1/20,_f),(1/2,_f),(1/4,_t)]
-- > in m_divisions_rq [1,1,1,1] d
--
-- > m_divisions_rq [1,1,1] [(4/7,_f),(33/28,_f),(9/20,_f),(4/5,_f)]
m_divisions_rq :: [RQ] -> [RQ_T] -> Either String [[RQ_T]]
m_divisions_rq z =
    fmap (rqt_tuplet_subdivide_seq_sanity_ (1/16) .
          map rqt_set_to_cmn) .
    rqt_separate z

-- | Variant of 'm_divisions_rq' that determines pulse divisions from
-- 'Time_Signature'.
--
-- > let d = [(4/7,_t),(1/7,_f),(2/7,_f)]
-- > in m_divisions_ts (1,4) d == Just [d]
--
-- > let d = map rq_rqt [1/3,1/6,2/5,1/10]
-- > in m_divisions_ts (1,4) d == Just [[(1/3,_f),(1/6,_f)]
-- >                                   ,[(2/5,_f),(1/10,_f)]]
--
-- > let d = map rq_rqt [4/7,33/28,9/20,4/5]
-- > in m_divisions_ts (3,4) d == Just [[(4/7,_f),(3/7,_t)]
-- >                                   ,[(3/4,_f),(1/4,_t)]
-- >                                   ,[(1/5,_f),(4/5,_f)]]
m_divisions_ts :: Time_Signature -> [RQ_T] -> Either String [[RQ_T]]
m_divisions_ts ts = m_divisions_rq (ts_divisions ts)

{-| Composition of 'to_measures_rq' and 'm_divisions_rq', where
measures are initially given as sets of divisions.

> let m = [[1,1,1],[1,1,1]]
> in to_divisions_rq m [2,2,2] == Right [[[(1,_t)],[(1,_f)],[(1,_t)]]
>                                      ,[[(1,_f)],[(1,_t)],[(1,_f)]]]

> let d = [2/7,1/7,4/7,5/7,8/7,1,1/7]
> in to_divisions_rq [[1,1,1,1]] d == Right [[[(2/7,_f),(1/7,_f),(4/7,_f)]
>                                           ,[(4/7,_t),(1/7,_f),(2/7,_t)]
>                                           ,[(6/7,_f),(1/7,_t)]
>                                           ,[(6/7,_f),(1/7,_f)]]]

> let d = [5/7,1,6/7,3/7]
> in to_divisions_rq [[1,1,1]] d == Right [[[(4/7,_t),(1/7,_f),(2/7,_t)]
>                                         ,[(4/7,_t),(1/7,_f),(2/7,_t)]
>                                         ,[(4/7,_f),(3/7,_f)]]]

> let d = [2/7,1/7,4/7,5/7,1,6/7,3/7]
> in to_divisions_rq [[1,1,1,1]] d == Right [[[(2/7,_f),(1/7,_f),(4/7,_f)]
>                                           ,[(4/7,_t),(1/7,_f),(2/7,_t)]
>                                           ,[(4/7,_t),(1/7,_f),(2/7,_t)]
>                                           ,[(4/7,_f),(3/7,_f)]]]

> let d = [4/7,33/28,9/20,4/5]
> in to_divisions_rq [[1,1,1]] d == Right [[[(4/7,_f),(3/7,_t)]
>                                          ,[(3/4,_f),(1/4,_t)]
>                                          ,[(1/5,_f),(4/5,_f)]]]

> let {p = [[1/2,1,1/2],[1/2,1]]
>     ;d = map (/6) [1,1,1,1,1,1,4,1,2,1,1,2,1,3]}
> in to_divisions_rq p d == Right [[[(1/6,_f),(1/6,_f),(1/6,_f)]
>                                  ,[(1/6,_f),(1/6,_f),(1/6,_f),(1/2,True)]
>                                  ,[(1/6,_f),(1/6,_f),(1/6,True)]]
>                                 ,[[(1/6,_f),(1/6,_f),(1/6,_f)]
>                                  ,[(1/3,_f),(1/6,_f),(1/2,_f)]]]

-}
to_divisions_rq :: [[RQ]] -> [RQ] -> Either String [[[RQ_T]]]
to_divisions_rq m x =
    let m' = map sum m
    in case to_measures_rq m' x of
         Right y -> all_right (zipWith m_divisions_rq m y)
         Left e -> Left e

-- | Variant of 'to_divisions_rq' with measures given as set of
-- 'Time_Signature'.
--
-- > let d = [3/5,2/5,1/3,1/6,7/10,17/15,1/2,1/6]
-- > in to_divisions_ts [(4,4)] d == Just [[[(3/5,_f),(2/5,_f)]
-- >                                       ,[(1/3,_f),(1/6,_f),(1/2,_t)]
-- >                                       ,[(1/5,_f),(4/5,_t)]
-- >                                       ,[(1/3,_f),(1/2,_f),(1/6,_f)]]]
--
-- > let d = [3/5,2/5,1/3,1/6,7/10,29/30,1/2,1/3]
-- > in to_divisions_ts [(4,4)] d == Just [[[(3/5,_f),(2/5,_f)]
-- >                                       ,[(1/3,_f),(1/6,_f),(1/2,_t)]
-- >                                       ,[(1/5,_f),(4/5,_t)]
-- >                                       ,[(1/6,_f),(1/2,_f),(1/3,_f)]]]
--
-- > let d = [3/5,2/5,1/3,1/6,7/10,4/5,1/2,1/2]
-- > in to_divisions_ts [(4,4)] d == Just [[[(3/5,_f),(2/5,_f)]
-- >                                       ,[(1/3,_f),(1/6,_f),(1/2,_t)]
-- >                                       ,[(1/5,_f),(4/5,_f)]
-- >                                       ,[(1/2,_f),(1/2,_f)]]]
--
-- > let d = [4/7,33/28,9/20,4/5]
-- > in to_divisions_ts [(3,4)] d == Just [[[(4/7,_f),(3/7,_t)]
-- >                                       ,[(3/4,_f),(1/4,_t)]
-- >                                       ,[(1/5,_f),(4/5,_f)]]]
to_divisions_ts :: [Time_Signature] -> [RQ] -> Either String [[[RQ_T]]]
to_divisions_ts ts = to_divisions_rq (map ts_divisions ts)

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
p_notate :: Bool -> [RQ_T] -> Either String [Duration_A]
p_notate z x =
    let f = p_simplify . rqt_to_duration_a z
        d = case p_tuplet_rqt x of
              Just (t,x') -> da_tuplet t (f x')
              Nothing -> f x
    in if rq_can_notate (map rqt_rq x)
       then Right d
       else Left (show ("p_notate",z,x))

-- | Notate measure.
--
-- > m_notate True [[(2/3,_f),(1/3,_t)],[(1,_t)],[(1,_f)]]
--
-- > let f = m_notate False . concat
--
-- > fmap f (to_divisions_ts [(4,4)] [3/5,2/5,1/3,1/6,7/10,17/15,1/2,1/6])
-- > fmap f (to_divisions_ts [(4,4)] [3/5,2/5,1/3,1/6,7/10,29/30,1/2,1/3])
m_notate :: Bool -> [[RQ_T]] -> Either String [Duration_A]
m_notate z m =
    let z' = z : map (is_tied_right . last) m
    in fmap concat (all_right (zipWith p_notate z' m))

{-| Multiple measure notation.

> let d = [2/7,1/7,4/7,5/7,8/7,1,1/7]
> in fmap mm_notate (to_divisions_ts [(4,4)] d)

> let d = [2/7,1/7,4/7,5/7,1,6/7,3/7]
> in fmap mm_notate (to_divisions_ts [(4,4)] d)

> let d = [3/5,2/5,1/3,1/6,7/10,4/5,1/2,1/2]
> in fmap mm_notate (to_divisions_ts [(4,4)] d)

> let {p = [[1/2,1,1/2],[1/2,1]]
>     ;d = map (/6) [1,1,1,1,1,1,4,1,2,1,1,2,1,3]}
> in fmap mm_notate (to_divisions_rq p d)

-}
mm_notate :: [[[RQ_T]]] -> Either String [[Duration_A]]
mm_notate d =
    let z = False : map (is_tied_right . last . last) d
    in all_right (zipWith m_notate z d)

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
-- > default_8_rule ((3,8),0,(1/2,1/2)) == True
-- > default_8_rule ((3,8),1/2,(1/2,1/2)) == True
-- > default_8_rule ((3,8),1,(1/2,1/2)) == True
-- > default_8_rule ((2,8),0,(1/2,1/2)) == True
-- > default_8_rule ((5,8),0,(1,1/2)) == True
-- > default_8_rule ((5,8),0,(2,1/2)) == True
default_8_rule :: Simplify_P
default_8_rule ((i,j),t,(p,q)) =
    let r = p + q
    in j == 8 &&
       denominator t `elem` [1,2] &&
       (r <= 2 || r == ts_rq (i,j) || rq_is_integral r)

-- | The default quarter note pulse simplifier rule.
--
-- > default_4_rule ((3,4),0,(1,1/2)) == True
-- > default_4_rule ((3,4),0,(1,3/4)) == True
-- > default_4_rule ((4,4),1,(1,1)) == False
-- > default_4_rule ((4,4),2,(1,1)) == True
-- > default_4_rule ((4,4),2,(1,2)) == True
-- > default_4_rule ((4,4),0,(2,1)) == True
-- > default_4_rule ((3,4),1,(1,1)) == False
default_4_rule :: Simplify_P
default_4_rule ((_,j),t,(p,q)) =
    let r = p + q
    in j == 4 &&
       denominator t == 1 &&
       even (numerator t) &&
       (r <= 2 || rq_is_integral r)

{-
-- | Any pulse-division aligned pair that sums to a division of the
-- pulse and does not cross a pulse boundary can be simplified.
--
-- > default_aligned_pulse_rule ((4,2),0,(2,1)) == True
-- > default_aligned_pulse_rule ((4,2),1,(1,1)) == False
-- > default_aligned_pulse_rule ((4,2),7,(4/10,1/10)) == True
default_aligned_pulse_rule :: Simplify_P
default_aligned_pulse_rule ((_,j),t,(p,q)) =
    let r = p + q
        w = whole_note_division_to_rq j
        tw = t `rq_mod` w
    in w `rq_mod` r == 0 &&
       t `rq_mod` (w `min` 1) == 0 &&
       (tw == 0 || tw + r <= w)
-}

-- | The default simplifier rule.  To extend provide a list of
-- 'Simplify_T'.
default_rule :: [Simplify_T] -> Simplify_P
default_rule x r = r `elem` x ||
                   {-default_aligned_pulse_rule r ||-}
                   default_4_rule r ||
                   default_8_rule r ||
                   default_table r

-- | Measure simplifier.  Apply given 'Simplify_P'.
m_simplify :: Simplify_P -> Time_Signature -> [Duration_A] -> [Duration_A]
m_simplify p ts =
    let f st (d0,a0) (d1,a1) =
            let t = Tie_Right `elem` a0 && Tie_Left `elem` a1
                e = End_Tuplet `notElem` a0 && not (any begins_tuplet a1)
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

{-| Notate RQ duration sequence.  Derive pulse divisions from
'Time_Signature' if not given directly.  Composition of
'to_divisions_ts', 'mm_notate' 'm_simplify'.

>  let ts = [(4,8),(3,8)]
>      ts_p = [[1/2,1,1/2],[1/2,1]]
>      rq = map (/6) [1,1,1,1,1,1,4,1,2,1,1,2,1,3]
>      sr x = T.default_rule [] x
>  in T.notate_rqp sr ts (Just ts_p) rq

-}
notate_rqp :: Simplify_P -> [Time_Signature] -> Maybe [[RQ]] -> [RQ] ->
              Either String [[Duration_A]]
notate_rqp r ts ts_p x = do
  let ts_p' = fromMaybe (map ts_divisions ts) ts_p
  mm <- to_divisions_rq ts_p' x
  dd <- mm_notate mm
  return (zipWith (m_simplify r) ts dd)

-- | Variant of 'notate_rqp' without pulse divisions (derive).
--
-- > notate (default_rule [((3,2),0,(2,2)),((3,2),0,(4,2))]) [(3,2)] [6]
notate :: Simplify_P -> [Time_Signature] -> [RQ] ->
          Either String [[Duration_A]]
notate r ts x = notate_rqp r ts Nothing x

-- * Ascribe

-- | Variant of 'zip' that retains elements of the right hand (rhs)
-- list where elements of the left hand (lhs) list meet the given lhs
-- predicate.  If the right hand side is longer the remaining elements
-- to be processed are given.  It is an error for the right hand side
-- to be short.
--
-- > zip_hold_lhs even [1..5] "abc" == ([],zip [1..6] "abbcc")
-- > zip_hold_lhs odd [1..6] "abc" == ([],zip [1..6] "aabbcc")
-- > zip_hold_lhs even [1] "ab" == ("b",[(1,'a')])
-- > zip_hold_lhs even [1,2] "a" == undefined
zip_hold_lhs :: (Show t,Show x) => (x -> Bool) -> [x] -> [t] -> ([t],[(x,t)])
zip_hold_lhs lhs_f =
    let f st e =
            case st of
              r:s -> let st' = if lhs_f e then st else s
                     in (st',(e,r))
              _ -> error (show ("zip_hold_lhs: rhs ends",st,e))
    in flip (mapAccumL f)

-- | Variant of 'zip_hold' that requires the right hand side to be
-- precisely the required length.
--
-- > zip_hold_lhs_err even [1..5] "abc" == zip [1..6] "abbcc"
-- > zip_hold_lhs_err odd [1..6] "abc" == zip [1..6] "aabbcc"
-- > zip_hold_lhs_err id [False,False] "a" == undefined
-- > zip_hold_lhs_err id [False] "ab" == undefined
zip_hold_lhs_err :: (Show t,Show x) => (x -> Bool) -> [x] -> [t] -> [(x,t)]
zip_hold_lhs_err lhs_f p q =
    case zip_hold_lhs lhs_f p q of
      ([],r) -> r
      e -> error (show ("zip_hold_lhs_err: lhs ends",e))

-- | Variant of 'zip' that retains elements of the right hand (rhs)
-- list where elements of the left hand (lhs) list meet the given lhs
-- predicate, and elements of the lhs list where elements of the rhs
-- meet the rhs predicate.  If the right hand side is longer the
-- remaining elements to be processed are given.  It is an error for
-- the right hand side to be short.
--
-- > zip_hold even (const False) [1..5] "abc" == ([],zip [1..6] "abbcc")
-- > zip_hold odd (const False) [1..6] "abc" == ([],zip [1..6] "aabbcc")
-- > zip_hold even (const False) [1] "ab" == ("b",[(1,'a')])
-- > zip_hold even (const False) [1,2] "a" == undefined
--
-- > zip_hold odd even [1,2,6] [1..5] == ([4,5],[(1,1),(2,1),(6,2),(6,3)])
zip_hold :: (Show t,Show x) => (x -> Bool) -> (t -> Bool) -> [x] -> [t] -> ([t],[(x,t)])
zip_hold lhs_f rhs_f =
    let f r x t =
            case (x,t) of
              ([],_) -> (t,reverse r)
              (_,[]) -> error "zip_hold: rhs ends"
              (x0:x',t0:t') -> let x'' = if rhs_f t0 then x else x'
                                   t'' = if lhs_f x0 then t else t'
                               in f ((x0,t0) : r) x'' t''
    in f []

-- | Zip a list of 'Duration_A' elements duplicating elements of the
-- right hand sequence for tied durations.
--
-- > let {Just d = to_divisions_ts [(4,4),(4,4)] [3,3,2]
-- >     ;f = map snd . snd . flip m_ascribe "xyz"}
-- > in fmap f (notate d) == Just "xxxyyyzz"
m_ascribe :: Show x => [Duration_A] -> [x] -> ([x],[(Duration_A,x)])
m_ascribe = zip_hold_lhs da_tied_right

-- | 'snd' '.' 'm_ascribe'.
ascribe :: Show x => [Duration_A] -> [x] -> [(Duration_A, x)]
ascribe d = snd . m_ascribe d

-- | Variant of 'm_ascribe' for a set of measures.
mm_ascribe :: Show x => [[Duration_A]] -> [x] -> [[(Duration_A,x)]]
mm_ascribe mm x =
    case mm of
      [] -> []
      m:mm' -> let (x',r) = m_ascribe m x
               in r : mm_ascribe mm' x'

-- | 'mm_ascribe of 'notate'.
notate_mm_ascribe :: Show a => [Simplify_T] -> [Time_Signature] -> Maybe [[RQ]] -> [RQ] -> [a] ->
                     Either String [[(Duration_A,a)]]
notate_mm_ascribe r ts rqp d p =
    let n = notate_rqp (default_rule r) ts rqp d
        f = flip mm_ascribe p
        err str = show ("notate_ascribe",str,ts,d,p)
    in either (Left . err) (Right . f) n

notate_mm_ascribe_err :: Show a => [Simplify_T] -> [Time_Signature] -> Maybe [[RQ]] -> [RQ] -> [a] ->
                         [[(Duration_A,a)]]
notate_mm_ascribe_err = either error id .:::: notate_mm_ascribe

-- | Group elements as /chords/ where a chord element is indicated by
-- the given predicate.
--
-- > group_chd even [1,2,3,4,4,5,7,8] == [[1,2],[3,4,4],[5],[7,8]]
group_chd :: (x -> Bool) -> [x] -> [[x]]
group_chd f x =
    case split (keepDelimsL (whenElt (not.f))) x of
      []:r -> r
      _ -> error "group_chd: first element chd?"

-- | Variant of 'ascribe' that groups the /rhs/ elements using
-- 'group_chd' and with the indicated /chord/ function, then rejoins
-- the resulting sequence.
ascribe_chd :: Show x => (x -> Bool) -> [Duration_A] -> [x] -> [(Duration_A, x)]
ascribe_chd chd_f d x =
    let x' = group_chd chd_f x
        jn (i,j) = zip (repeat i) j
    in concatMap jn (ascribe d x')

-- | Variant of 'mm_ascribe' using 'group_chd'
mm_ascribe_chd :: Show x => (x -> Bool) -> [[Duration_A]] -> [x] -> [[(Duration_A,x)]]
mm_ascribe_chd chd_f d x =
    let x' = group_chd chd_f x
        jn (i,j) = zip (repeat i) j
    in map (concatMap jn) (mm_ascribe d x')
