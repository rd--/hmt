-- | Basic temporal sequence functions.
module Music.Theory.Time.Seq where

import Data.Bifunctor {- base -}
import Data.Function {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}

import qualified Safe {- safe -}

import qualified Data.List.Ordered {- data-ordlist -}
import qualified Data.Map as Map {- containers -}

import qualified Music.Theory.List as List {- hmt-base -}
import qualified Music.Theory.Math as Math {- hmt-base -}
import qualified Music.Theory.Ord as Ord {- hmt-base -}
import qualified Music.Theory.Tuple as Tuple {- hmt-base -}

-- * Types

-- | Sequence of elements of type /a/ each with uniform duration of type /t/.
type Useq t a = (t, [a])

{- | Duration sequence.
/t/ indicates the /forward/ duration of the value, ie. the interval to the next value.
If there are other durations they must be encoded at /a/.
If the sequence does not begin at time zero there must be an /empty/ value for /a/.
-}
type Dseq t a = [(t, a)]

{- | Inter-offset sequence.
/t/ is the interval /before/ the value.
Duration can be encoded at /a/, or if implicit /a/ must include an end of sequence value.
-}
type Iseq t a = [(t, a)]

{- | Pattern sequence.
The duration is a triple of /logical/, /sounding/ and /forward/ durations.
These indicate the time the value conceptually takes, the time it actually takes, and the time to the next event.
If the sequence does not begin at time zero there must be an /empty/ value for /a/.
-}
type Pseq t a = [((t, t, t), a)]

{- | Time-point sequence.
/t/ is the start time of the value.
To express holes /a/ must have an /empty/ value.
Duration can be encoded at /a/, or if implicit /a/ must include an end of sequence value.
-}
type Tseq t a = [(t, a)]

{- | Window sequence.
/t/ is a duple of /start-time/ and /duration/.
Holes exist where /start-time(n) + duration(n) < start-time(n + 1)/.
Overlaps exist where the same relation is '>'.
-}
type Wseq t a = [((t, t), a)]

{- | Event sequence.
/t/ is a triple of /start-time/, /duration/ and /length/.
/length/ isn't necessarily the time to the next event, though ordinarily it should not be greater than that interval.
-}
type Eseq t a = [((t, t, t), a)]

-- * Zip

-- | Construct 'Pseq'.
pseq_zip :: [t] -> [t] -> [t] -> [a] -> Pseq t a
pseq_zip l o f = zip (zip3 l o f)

-- | Construct 'Wseq'.
wseq_zip :: [t] -> [t] -> [a] -> Wseq t a
wseq_zip t d = zip (zip t d)

-- * Time span

{- | Given functions for deriving start and end times calculate time span of sequence.
Requires sequence be finite.

>>> seq_tspan id id []
(0,0)

>>> seq_tspan id id (zip [0..9] ['a'..])
(0,9)
-}
seq_tspan :: Num n => (t -> n) -> (t -> n) -> [(t, a)] -> (n, n)
seq_tspan st et sq =
  ( maybe 0 (st . fst) (Safe.headMay sq)
  , maybe 0 (et . fst) (Safe.lastMay sq)
  )

-- | 'seq_tspan' for 'Tseq'.
tseq_tspan :: Num t => Tseq t a -> (t, t)
tseq_tspan = seq_tspan id id

-- | 'seq_tspan' for 'Wseq'.
wseq_tspan :: Num t => Wseq t a -> (t, t)
wseq_tspan = seq_tspan fst (uncurry (+))

{- | Start time of sequence.

>>> wseq_start [((1,2),'a')]
1

>>> wseq_start []
0
-}
wseq_start :: Num t => Wseq t a -> t
wseq_start = fst . wseq_tspan

{- | End time of sequence.

>>> wseq_end [((1,2),'a')]
3

>>> wseq_end (useq_to_wseq 0 (1,"linear"))
6
-}
wseq_end :: Num t => Wseq t a -> t
wseq_end = snd . wseq_tspan

-- * Duration

-- | Sum durations at 'Dseq', result is the end time of the last element.
dseq_dur :: Num t => Dseq t a -> t
dseq_dur = sum . map fst

-- | Sum durations at 'Iseq', result is the start time of the last element.
iseq_dur :: Num t => Iseq t a -> t
iseq_dur = sum . map fst

-- | Sum durations at 'Pseq', result is the end time of the last element.
pseq_dur :: Num t => Pseq t a -> t
pseq_dur = sum . map (Tuple.t3_third . fst)

{- | The interval of 'tseq_tspan', ie. from the start of the first element to the start of the last.

>>> tseq_dur (zip [0..] "abcde|")
5
-}
tseq_dur :: Num t => Tseq t a -> t
tseq_dur = uncurry subtract . tseq_tspan

{- | The interval of 'wseq_tspan', ie. from the start of the first element to the end of the last.

>>> wseq_dur (zip (zip [0..] (repeat 2)) "abcde")
6
-}
wseq_dur :: Num t => Wseq t a -> t
wseq_dur = uncurry subtract . wseq_tspan

-- * Window

-- | Prefix of sequence where the start time precedes or is at the indicated time.
wseq_until :: Ord t => t -> Wseq t a -> Wseq t a
wseq_until tm = takeWhile (\((t0, _), _) -> t0 <= tm)

{- | Keep only elements that are entirely contained within the indicated temporal window,
which is inclusive at the left & right edges, ie. [t0,t1].
Halts processing at end of window.

>>> wseq_twindow (5,9) (zip (zip [1..] (repeat 1)) ['a'..])
[((5,1),'e'),((6,1),'f'),((7,1),'g'),((8,1),'h')]

>>> wseq_twindow (1,2) [((1,1),'a'),((1,2),'b')]
[((1,1),'a')]
-}
wseq_twindow :: (Num t, Ord t) => (t, t) -> Wseq t a -> Wseq t a
wseq_twindow (w0, w1) =
  let f (st, du) = w0 <= st && (st + du) <= w1
  in wseq_tfilter f . wseq_until w1

{- | Select nodes that are active at indicated time, comparison is inclusive at left and exclusive at right.
Halts processing at end of window.

>>> let sq = [((1,1),'a'),((1,2),'b')]
>>> map (wseq_at sq) [1,2] == [sq,[((1,2),'b')]]
True

>>> wseq_at (zip (zip [1..] (repeat 1)) ['a'..]) 3
[((3,1),'c')]
-}
wseq_at :: (Num t, Ord t) => Wseq t a -> t -> Wseq t a
wseq_at sq tm =
  let sel ((t0, t1), _) = t0 <= tm && tm < (t0 + t1)
      end ((t0, _), _) = t0 <= tm
  in filter sel (takeWhile end sq)

{- | Select nodes that are active within the indicated window, comparison is inclusive at left and exclusive at right.
Halts processing at end of window.

>>> let sq = [((0,2),'a'),((0,4),'b'),((2,4),'c')]
>>> wseq_at_window sq (1,3) == sq
True

>>> wseq_at_window (zip (zip [1..] (repeat 1)) ['a'..]) (3,4)
[((3,1),'c'),((4,1),'d')]
-}
wseq_at_window :: (Num t, Ord t) => Wseq t a -> (t, t) -> Wseq t a
wseq_at_window sq (w0, w1) =
  let f (t0, t1) t = t0 <= t && t < t1
      g (st, du) = let w = (st, st + du) in f w w0 || f w w1
  in wseq_tfilter g (wseq_until w1 sq)

-- * Append

-- | Type specialised '++'
dseq_append :: Dseq t a -> Dseq t a -> Dseq t a
dseq_append = (++)

-- | Type specialised '++'
iseq_append :: Iseq t a -> Iseq t a -> Iseq t a
iseq_append = (++)

-- | Type specialised '++'
pseq_append :: Pseq t a -> Pseq t a -> Pseq t a
pseq_append = (++)

-- * Merge

-- | Merge comparing only on time.
tseq_merge :: Ord t => Tseq t a -> Tseq t a -> Tseq t a
tseq_merge = Data.List.Ordered.mergeBy (compare `on` fst)

-- | Merge, where times are equal compare values.
tseq_merge_by :: Ord t => List.Compare_F a -> Tseq t a -> Tseq t a -> Tseq t a
tseq_merge_by cmp = List.merge_by_two_stage fst cmp snd

{- | Merge, where times are equal apply /f/ to form a single value.

>>> let p = zip [1,3,5] "abc"
>>> let q = zip [1,2,3] "ABC"
>>> tseq_merge_resolve (\x _ -> x) p q
[(1,'a'),(2,'B'),(3,'b'),(5,'c')]

>>> tseq_merge_resolve (\_ x -> x) p q
[(1,'A'),(2,'B'),(3,'C'),(5,'c')]
-}
tseq_merge_resolve :: Ord t => (a -> a -> a) -> Tseq t a -> Tseq t a -> Tseq t a
tseq_merge_resolve f =
  let cmp = compare `on` fst
      g (t, p) (_, q) = (t, f p q)
  in List.merge_by_resolve g cmp

-- | Compare first by start time, then by duration.
w_compare :: Ord t => ((t, t), a) -> ((t, t), a) -> Ordering
w_compare ((t1, d1), _) ((t2, d2), _) =
  case compare t1 t2 of
    EQ -> compare d1 d2
    r -> r

-- | Merge considering only start times.
wseq_merge :: Ord t => Wseq t a -> Wseq t a -> Wseq t a
wseq_merge = Data.List.Ordered.mergeBy (compare `on` (fst . fst))

-- | Merge set considering both start times & durations.
wseq_merge_set :: Ord t => [Wseq t a] -> Wseq t a
wseq_merge_set = List.merge_set_by w_compare

-- | Merge considering only start times.
eseq_merge :: Ord t => Eseq t a -> Eseq t a -> Eseq t a
eseq_merge = Data.List.Ordered.mergeBy (compare `on` (Tuple.t3_fst . fst))

-- * Lookup

-- | Locate nodes to the left and right of indicated time.
tseq_lookup_window_by :: (t -> t -> Ordering) -> Tseq t e -> t -> (Maybe (t, e), Maybe (t, e))
tseq_lookup_window_by cmp =
  let recur l sq t =
        case sq of
          [] -> (l, Nothing)
          (t', e) : sq' -> case cmp t t' of
            LT -> (l, Just (t', e))
            _ -> case sq' of
              [] -> (Just (t', e), Nothing)
              (t'', e') : _ -> case cmp t t'' of
                LT -> (Just (t', e), Just (t'', e'))
                _ -> recur (Just (t', e)) sq' t
  in recur Nothing

tseq_lookup_active_by :: (t -> t -> Ordering) -> Tseq t e -> t -> Maybe e
tseq_lookup_active_by cmp sq = fmap snd . fst . tseq_lookup_window_by cmp sq

tseq_lookup_active :: Ord t => Tseq t e -> t -> Maybe e
tseq_lookup_active = tseq_lookup_active_by compare

tseq_lookup_active_by_def :: e -> (t -> t -> Ordering) -> Tseq t e -> t -> e
tseq_lookup_active_by_def def cmp sq = fromMaybe def . tseq_lookup_active_by cmp sq

tseq_lookup_active_def :: Ord t => e -> Tseq t e -> t -> e
tseq_lookup_active_def def = tseq_lookup_active_by_def def compare

-- * Lseq

-- | Iterpolation type enumeration.
data Interpolation_T
  = None
  | Linear
  deriving (Eq, Enum, Show)

-- | Variant of 'Tseq' where nodes have an 'Intepolation_T' value.
type Lseq t a = Tseq (t, Interpolation_T) a

{- | Linear interpolation.
     The Real constraint on t is to allow conversion from t to e (realToFrac).
-}
lerp :: (Fractional t, Real t, Fractional e) => (t, e) -> (t, e) -> t -> e
lerp (t0, e0) (t1, e1) t =
  let n = t1 - t0
      m = t - t0
      l = m / n
  in realToFrac l * (e1 - e0) + e0

-- | Temporal map.
lseq_tmap :: (t -> t') -> Lseq t a -> Lseq t' a
lseq_tmap f = let g ((t, i), e) = ((f t, i), e) in map g

{- | This can give 'Nothing' if /t/ precedes the 'Lseq' or
if /t/ is after the final element of 'Lseq' and that element has an interpolation type other than 'None'.
-}
lseq_lookup :: (Fractional t, Real t, Fractional e) => (t -> t -> Ordering) -> Lseq t e -> t -> Maybe e
lseq_lookup cmp sq t =
  case tseq_lookup_window_by (cmp `on` fst) sq (t, undefined) of
    (Nothing, _) -> Nothing
    (Just ((_, None), e), _) -> Just e
    (Just ((t0, Linear), e0), Just ((t1, _), e1)) -> Just (lerp (t0, e0) (t1, e1) t)
    _ -> Nothing

-- | 'error'ing variant.
lseq_lookup_err :: (Fractional t, Real t, Fractional e) => (t -> t -> Ordering) -> Lseq t e -> t -> e
lseq_lookup_err cmp sq = fromMaybe (error "lseq_lookup") . lseq_lookup cmp sq

-- * Map, Filter, Find

-- | 'map' over time (/t/) data.
seq_tmap :: (t1 -> t2) -> [(t1, a)] -> [(t2, a)]
seq_tmap f = map (first f)

-- | 'map' over element (/e/) data.
seq_map :: (e1 -> e2) -> [(t, e1)] -> [(t, e2)]
seq_map f = map (second f)

{- | 'map' /t/ and /e/ simultaneously.

>>> seq_bimap negate succ (zip [1..5] [0..4])
[(-1,1),(-2,2),(-3,3),(-4,4),(-5,5)]
-}
seq_bimap :: (t1 -> t2) -> (e1 -> e2) -> [(t1, e1)] -> [(t2, e2)]
seq_bimap f = map . bimap f

-- | 'filter' over time (/t/) data.
seq_tfilter :: (t -> Bool) -> [(t, a)] -> [(t, a)]
seq_tfilter f = filter (f . fst)

-- | 'filter' over element (/e/) data.
seq_filter :: (b -> Bool) -> [(a, b)] -> [(a, b)]
seq_filter f = filter (f . snd)

-- | 'find' over element (/e/) data.
seq_find :: (e -> Bool) -> [(t, e)] -> Maybe (t, e)
seq_find f = find (f . snd)

-- * Maybe

-- | 'mapMaybe' variant.
seq_map_maybe :: (p -> Maybe q) -> [(t, p)] -> [(t, q)]
seq_map_maybe f =
  let g (t, e) = fmap (\e' -> (t, e')) (f e)
  in mapMaybe g

-- | Variant of 'catMaybes'.
seq_cat_maybes :: [(t, Maybe q)] -> [(t, q)]
seq_cat_maybes = seq_map_maybe id

-- | If value is unchanged at subsequent entry, according to /f/, replace with 'Nothing'.
seq_changed_by :: (a -> a -> Bool) -> [(t, a)] -> [(t, Maybe a)]
seq_changed_by f l =
  let recur z sq =
        case sq of
          [] -> []
          (t, e) : sq' ->
            if f e z
              then (t, Nothing) : recur z sq'
              else (t, Just e) : recur e sq'
  in case l of
      [] -> []
      (t, e) : l' -> (t, Just e) : recur e l'

{- | 'seq_changed_by' '=='.

>>> seq_cat_maybes (seq_changed (zip [1..] "sttrrinng"))
[(1,'s'),(2,'t'),(4,'r'),(6,'i'),(7,'n'),(9,'g')]
-}
seq_changed :: Eq a => [(t, a)] -> [(t, Maybe a)]
seq_changed = seq_changed_by (==)

-- * Specialised temporal maps.

-- | Apply /f/ at time points of 'Wseq'.
wseq_tmap_st :: (t -> t) -> Wseq t a -> Wseq t a
wseq_tmap_st f = seq_tmap (first f)

-- | Apply /f/ at durations of elements of 'Wseq'.
wseq_tmap_dur :: (t -> t) -> Wseq t a -> Wseq t a
wseq_tmap_dur f = seq_tmap (second f)

-- * Partition

-- | Given a function that determines a /voice/ for a value, partition a sequence into voices.
seq_partition :: Ord v => (a -> v) -> [(t, a)] -> [(v, [(t, a)])]
seq_partition voice sq =
  let assign m (t, a) = Map.insertWith (++) (voice a) [(t, a)] m
      from_map =
        sortOn fst
          . map (second reverse)
          . Map.toList
  in from_map (foldl assign Map.empty sq)

{- | Type specialised 'seq_partition'.

>>> let p = zip [0,1,3,5] (zip (repeat 0) "abcd")
>>> let q = zip [2,4,6,7] (zip (repeat 1) "ABCD")
>>> tseq_partition fst (tseq_merge p q) == [(0,p),(1,q)]
True
-}
tseq_partition :: Ord v => (a -> v) -> Tseq t a -> [(v, Tseq t a)]
tseq_partition = seq_partition

-- | Type specialised 'seq_partition'.
wseq_partition :: Ord v => (a -> v) -> Wseq t a -> [(v, Wseq t a)]
wseq_partition = seq_partition

-- * Coalesce

{- | Given a decision predicate and a join function, recursively join adjacent elements.

>>> coalesce_f undefined undefined []
[]

>>> coalesce_f (==) const "abbcccbba"
"abcba"

>>> coalesce_f (==) (+) [1,2,2,3,3,3]
[1,4,6,3]
-}
coalesce_f :: (t -> t -> Bool) -> (t -> t -> t) -> [t] -> [t]
coalesce_f dec_f jn_f z =
  let recur p l =
        case l of
          [] -> [p]
          c : l' ->
            if dec_f p c
              then recur (jn_f p c) l'
              else p : recur c l'
  in case z of
      [] -> []
      e0 : z' -> recur e0 z'

-- | 'coalesce_f' using 'mappend' for the join function.
coalesce_m :: Monoid t => (t -> t -> Bool) -> [t] -> [t]
coalesce_m dec_f = coalesce_f dec_f mappend

-- | Form of 'coalesce_t' where the join predicate is on the /element/ only, the /times/ are summed.
coalesce_t :: Num t => ((t, a) -> (t, a) -> Bool) -> (a -> a -> a) -> [(t, a)] -> [(t, a)]
coalesce_t dec_f jn_f = coalesce_f dec_f (\(t1, a1) (t2, a2) -> (t1 + t2, jn_f a1 a2))

{- | Form of 'coalesce_f' where both the decision and join predicates are on the/element/, the /times/ are summed.

>>> seq_coalesce (==) const (useq_to_dseq (1,"abbcccdde"))
[(1,'a'),(2,'b'),(3,'c'),(2,'d'),(1,'e')]
-}
seq_coalesce :: Num t => (a -> a -> Bool) -> (a -> a -> a) -> [(t, a)] -> [(t, a)]
seq_coalesce dec_f jn_f = coalesce_t (dec_f `on` snd) jn_f

dseq_coalesce :: Num t => (a -> a -> Bool) -> (a -> a -> a) -> Dseq t a -> Dseq t a
dseq_coalesce = seq_coalesce

{- | Given /equality/ predicate, simplify sequence by summing durations of adjacent /equal/ elements.
This is a special case of 'dseq_coalesce' where the /join/ function is 'const'.
The implementation is simpler and non-recursive.

>>> let d = useq_to_dseq (1,"abbcccdde")
>>> let r = dseq_coalesce (==) const d
>>> dseq_coalesce' (==) d == r
True
-}
dseq_coalesce' :: Num t => (a -> a -> Bool) -> Dseq t a -> Dseq t a
dseq_coalesce' eq =
  let f l = let (t, e) = unzip l in (sum t, List.head_err e)
  in map f . groupBy (eq `on` snd)

iseq_coalesce :: Num t => (a -> a -> Bool) -> (a -> a -> a) -> Iseq t a -> Iseq t a
iseq_coalesce = seq_coalesce

-- * T-coalesce

seq_tcoalesce :: (t -> t -> Bool) -> (a -> a -> a) -> [(t, a)] -> [(t, a)]
seq_tcoalesce eq_f jn_f =
  let dec_f = eq_f `on` fst
      jn_f' (t, a1) (_, a2) = (t, jn_f a1 a2)
  in coalesce_f dec_f jn_f'

tseq_tcoalesce :: Eq t => (a -> a -> a) -> Tseq t a -> Tseq t a
tseq_tcoalesce = seq_tcoalesce (==)

-- | Type specialised 'seq_tcoalesce'.
wseq_tcoalesce :: ((t, t) -> (t, t) -> Bool) -> (a -> a -> a) -> Wseq t a -> Wseq t a
wseq_tcoalesce = seq_tcoalesce

-- * Group

{- | Post-process 'groupBy' of /cmp/ 'on' 'fst'.

>>> group_f (==) (zip [0,1,1,2,2,3] ['a'..])
[(0,"a"),(1,"bc"),(2,"de"),(3,"f")]
-}
group_f :: (Eq t, Num t) => (t -> t -> Bool) -> [(t, a)] -> [(t, [a])]
group_f cmp =
  let f l =
        let (t, a) = unzip l
        in case t of
            [] -> error "group_f: []?"
            t0 : _ -> (t0, a)
  in map f . groupBy (cmp `on` fst)

{- | Group values at equal time points.

>>> tseq_group (zip [0,1,1,2,2,3] ['a'..])
[(0,"a"),(1,"bc"),(2,"de"),(3,"f")]

>>> tseq_group [(1,'a'),(1,'b')]
[(1,"ab")]

>>> tseq_group [(1,'a'),(2,'b'),(2,'c')]
[(1,"a"),(2,"bc")]
-}
tseq_group :: (Eq t, Num t) => Tseq t a -> Tseq t [a]
tseq_group = group_f (==)

{- | Group values where the inter-offset time is @0@ to the left.

>>> iseq_group (zip [0,1,0,0,1,0] ['a'..])
[(0,"a"),(1,"bcd"),(1,"ef")]
-}
iseq_group :: (Eq t, Num t) => Iseq t a -> Iseq t [a]
iseq_group = group_f (\_ d -> d == 0)

-- * Fill

{- | Set durations so that there are no gaps or overlaps.
For entries with the same start time this leads to zero durations.

>>> let r = wseq_zip [0,3,3,5] [3,0,2,1] "abcd"
>>> wseq_fill_dur (wseq_zip [0,3,3,5] [2,1,2,1] "abcd") == r
True
-}
wseq_fill_dur :: Num t => Wseq t a -> Wseq t a
wseq_fill_dur l =
  let f (((t1, _), e), ((t2, _), _)) = ((t1, t2 - t1), e)
  in map f (List.adj2 1 l) ++ [last l]

-- * Dseq

dseq_lcm :: Dseq Rational e -> Integer
dseq_lcm = foldl1 lcm . map (denominator . fst)

-- | Scale by lcm so that all durations are integral.
dseq_set_whole :: [Dseq Rational e] -> [Dseq Integer e]
dseq_set_whole sq =
  let m = maximum (map dseq_lcm sq)
      t_f n = Math.rational_whole_err (n * fromIntegral m)
  in map (dseq_tmap t_f) sq

-- | End-time of sequence (ie. sum of durations).
dseq_end :: Num t => Dseq t a -> t
dseq_end = sum . map fst

-- * Tseq

{- | Given a a default value, a 'Tseq' /sq/ and a list of time-points /t/,
generate a Tseq that is a union of the timepoints at /sq/ and /t/
where times in /t/ not at /sq/ are given the /current/ value,
or /def/ if there is no value.

>>> tseq_latch 'a' [(2,'b'),(4,'c')] [1..5] == zip [1..5] "abbcc"
True
-}
tseq_latch :: Ord t => a -> Tseq t a -> [t] -> Tseq t a
tseq_latch def sq t =
  case (sq, t) of
    ([], _) -> zip t (repeat def)
    (_, []) -> []
    ((sq_t, sq_e) : sq', t0 : t') -> case compare sq_t t0 of
      LT -> (sq_t, sq_e) : tseq_latch sq_e sq' t
      EQ -> (sq_t, sq_e) : tseq_latch sq_e sq' t'
      GT -> (t0, def) : tseq_latch def sq t'

-- | End-time of sequence (ie. time of last event).
tseq_end :: Tseq t a -> t
tseq_end = fst . last

-- | Append the value /nil/ at /n/ seconds after the end of the sequence.
tseq_add_nil_after :: Num t => a -> t -> Tseq t a -> Tseq t a
tseq_add_nil_after nil n sq = sq ++ [(tseq_end sq + n, nil)]

-- * Wseq

{- | Sort 'Wseq' by start time, 'Wseq' ought never to be out of order.

>>> wseq_sort [((3,1),'a'),((1,3),'b')]
[((1,3),'b'),((3,1),'a')]
-}
wseq_sort :: Ord t => Wseq t a -> Wseq t a
wseq_sort = sortBy (compare `on` (fst . fst))

-- | Transform 'Wseq' to 'Tseq' by discarding durations.
wseq_discard_dur :: Wseq t a -> Tseq t a
wseq_discard_dur = let f ((t, _), e) = (t, e) in map f

{- | Are /e/ equal and do nodes overlap?
Nodes are ascending, and so overlap if:
1. they begin at the same time and the first has non-zero duration, or
2. the second begins before the first ends.
-}
wseq_nodes_overlap :: (Ord t, Num t) => (e -> e -> Bool) -> ((t, t), e) -> ((t, t), e) -> Bool
wseq_nodes_overlap eq_f ((t1, d1), a1) ((t2, _d2), a2) =
  eq_f a1 a2 && ((t1 == t2 && d1 > 0) || (t2 < (t1 + d1)))

{- | Find first node at /sq/ that overlaps with /e0/, if there is one.
Note: this could, but does not, halt early, ie. when t2 > (t1 + d1).
-}
wseq_find_overlap_1 :: (Ord t, Num t) => (e -> e -> Bool) -> ((t, t), e) -> Wseq t e -> Bool
wseq_find_overlap_1 eq_f e0 = isJust . find (wseq_nodes_overlap eq_f e0)

{- | Determine if sequence has any overlapping equal nodes, stops after finding first instance.

>>> wseq_has_overlaps (==) []
False

>>> wseq_has_overlaps (==) [((0,1),'x')]
False
-}
wseq_has_overlaps :: (Ord t, Num t) => (e -> e -> Bool) -> Wseq t e -> Bool
wseq_has_overlaps eq_fn =
  let recur sq =
        case sq of
          [] -> False
          e0 : sq' -> wseq_find_overlap_1 eq_fn e0 sq' || recur sq'
  in recur

{- | Remove overlaps by deleting any overlapping nodes.

>>> let sq = [((0,1),'a'),((0,5),'a'),((1,5),'a'),((3,1),'a')]
>>> wseq_has_overlaps (==) sq
True

>>> let sq_rw = wseq_remove_overlaps_rm (==) sq
>>> sq_rw
[((0,1),'a'),((1,5),'a')]

>>> wseq_has_overlaps (==) sq_rw
False
-}
wseq_remove_overlaps_rm :: (Ord t, Num t) => (e -> e -> Bool) -> Wseq t e -> Wseq t e
wseq_remove_overlaps_rm eq_f =
  let recur sq =
        case sq of
          [] -> []
          e0 : sq' -> e0 : recur (filter (not . wseq_nodes_overlap eq_f e0) sq')
  in recur

{- | Find first instance of overlap of /e/ at /sq/ and re-write durations so nodes don't overlap.
     If equal nodes begin simultaneously delete the shorter node (eithe LHS or RHS).
     If a node extends into a later node shorten the initial (LHS) duration (apply /dur_fn/ to iot).
-}
wseq_remove_overlap_rw_1 ::
  (Ord t, Num t) =>
  (e -> e -> Bool) ->
  (t -> t) ->
  ((t, t), e) ->
  Wseq t e ->
  Maybe (Wseq t e)
wseq_remove_overlap_rw_1 eq_f dur_fn ((t, d), a) sq =
  let n_eq ((t1, d1), e1) ((t2, d2), e2) = t1 == t2 && d1 == d2 && eq_f e1 e2
  in case find (eq_f a . snd) sq of
      Nothing -> Nothing
      Just ((t', d'), a') ->
        if t == t'
          then
            if d <= d'
              then Just sq -- delete LHS
              else Just (((t, d), a) : deleteBy n_eq ((t', d'), a') sq) -- delete RHS
          else
            if t' < t + d
              then Just (((t, dur_fn (t' - t)), a) : sq) -- truncate LHS
              else Nothing

{- | Run 'wseq_remove_overlap_rw_1' until sequence has no overlaps.

>>> let sq = [((0,1),'a'),((0,5),'a'),((1,5),'a'),((3,1),'a')]
>>> wseq_has_overlaps (==) sq
True

>>> let sq_rw = wseq_remove_overlaps_rw (==) id sq
>>> sq_rw
[((0,1),'a'),((1,2),'a'),((3,1),'a')]

>>> wseq_has_overlaps (==) sq_rw
False

> import qualified Music.Theory.Array.Csv.Midi.Mnd as Mnd
> let csv_fn = "/home/rohan/uc/the-center-is-between-us/visitants/csv/midi/air.B.1.csv"
> sq <- Mnd.csv_midi_read_wseq csv_fn :: IO (Wseq Double (Mnd.Event Double))
> length sq == 186
> length (wseq_remove_overlaps_rw (==) id sq) == 183
-}
wseq_remove_overlaps_rw :: (Ord t, Num t) => (e -> e -> Bool) -> (t -> t) -> Wseq t e -> Wseq t e
wseq_remove_overlaps_rw eq_f dur_fn =
  let recur sq =
        case sq of
          [] -> []
          h : sq' ->
            case wseq_remove_overlap_rw_1 eq_f dur_fn h sq' of
              Nothing -> h : recur sq'
              Just sq'' -> recur sq''
  in recur

-- | Unjoin elements (assign equal time stamps to all elements).
seq_unjoin :: [(t, [e])] -> [(t, e)]
seq_unjoin = let f (t, e) = zip (repeat t) e in concatMap f

-- | Type specialised 'seq_unjoin'.
wseq_unjoin :: Wseq t [e] -> Wseq t e
wseq_unjoin = seq_unjoin

{- | Shift (displace) onset times by /i/.

>>> wseq_shift 3 [((1,2),'a')]
[((4,2),'a')]
-}
wseq_shift :: Num t => t -> Wseq t a -> Wseq t a
wseq_shift i = wseq_tmap_st (+ i)

{- | Shift q to end of p and append.

>>> wseq_append [((1,2),'a')] [((1,2),'b')]
[((1,2),'a'),((4,2),'b')]
-}
wseq_append :: Num t => Wseq t a -> Wseq t a -> Wseq t a
wseq_append p q = p ++ wseq_shift (wseq_end p) q

{- | 'foldl1' of 'wseq_append'

>>> wseq_concat [[((1,2),'a')],[((1,2),'b')]]
[((1,2),'a'),((4,2),'b')]
-}
wseq_concat :: Num t => [Wseq t a] -> Wseq t a
wseq_concat = foldl1 wseq_append

-- | Transform sequence to start at time zero.
wseq_zero :: Num t => Wseq t a -> Wseq t a
wseq_zero sq = let t0 = wseq_start sq in wseq_tmap (\(st, du) -> (st - t0, du)) sq

-- * Begin/End

-- | Container to mark the /begin/ and /end/ of a value.
data Begin_End a = Begin a | End a deriving (Eq, Show)

-- | Functor instance.
begin_end_map :: (t -> u) -> Begin_End t -> Begin_End u
begin_end_map f x =
  case x of
    Begin a -> Begin (f a)
    End a -> End (f a)

instance Functor Begin_End where fmap = begin_end_map

-- | Structural comparison at 'Begin_End', 'Begin' compares less than 'End'.
cmp_begin_end :: Begin_End a -> Begin_End b -> Ordering
cmp_begin_end p q =
  case (p, q) of
    (Begin _, End _) -> LT
    (Begin _, Begin _) -> EQ
    (End _, End _) -> EQ
    (End _, Begin _) -> GT

-- instance Eq t => Ord (Begin_End t) where compare = cmp_begin_end

-- | Translate container types.
either_to_begin_end :: Either a a -> Begin_End a
either_to_begin_end p =
  case p of
    Left a -> Begin a
    Right a -> End a

-- | Translate container types.
begin_end_to_either :: Begin_End a -> Either a a
begin_end_to_either p =
  case p of
    Begin a -> Left a
    End a -> Right a

-- | Equivalent to 'partitionEithers'.
begin_end_partition :: [Begin_End a] -> ([a], [a])
begin_end_partition =
  let f e (p, q) = case e of
        Begin x -> (x : p, q)
        End x -> (p, x : q)
  in foldr f ([], [])

-- | Add or delete element from accumulated state given equality function.
begin_end_track_by :: (a -> a -> Bool) -> [a] -> Begin_End a -> [a]
begin_end_track_by eq_f st e =
  case e of
    Begin x -> x : st
    End x -> deleteBy eq_f x st

-- | 'begin_end_track_by' of '=='.
begin_end_track :: Eq a => [a] -> Begin_End a -> [a]
begin_end_track = begin_end_track_by (==)

{- | Convert 'Wseq' to 'Tseq' transforming elements to 'Begin_End'.
     When merging, /end/ elements precede /begin/ elements at equal times.

>>> wseq_begin_end [((0,5),'a'),((2,2),'b')]
[(0,Begin 'a'),(2,Begin 'b'),(4,End 'b'),(5,End 'a')]

>>> wseq_begin_end [((0,1),'a'),((1,1),'b'),((2,1),'c')]
[(0,Begin 'a'),(1,End 'a'),(1,Begin 'b'),(2,End 'b'),(2,Begin 'c'),(3,End 'c')]
-}
wseq_begin_end :: (Num t, Ord t) => Wseq t a -> Tseq t (Begin_End a)
wseq_begin_end sq =
  let f ((t, d), a) = [(t, Begin a), (t + d, End a)]
      g l =
        case l of
          [] -> []
          e : l' -> tseq_merge_by (\x -> Ord.ord_invert . cmp_begin_end x) e (g l')
  in g (map f sq)

-- | 'begin_end_to_either' of 'wseq_begin_end'.
wseq_begin_end_either :: (Num t, Ord t) => Wseq t a -> Tseq t (Either a a)
wseq_begin_end_either = tseq_map begin_end_to_either . wseq_begin_end

{- | Variant that applies /begin/ and /end/ functions to nodes.

>>> wseq_begin_end_f Data.Char.toUpper id [((0,5),'a'),((2,2),'b')]
[(0,'A'),(2,'B'),(4,'b'),(5,'a')]
-}
wseq_begin_end_f :: (Ord t, Num t) => (a -> b) -> (a -> b) -> Wseq t a -> Tseq t b
wseq_begin_end_f f g = tseq_map (either f g) . wseq_begin_end_either

{- | Generate for each time-point the triple (begin-list,end-list,hold-list).
The elements of the end-list have been deleted from the hold list.
-}
tseq_begin_end_accum :: Eq a => Tseq t [Begin_End a] -> Tseq t ([a], [a], [a])
tseq_begin_end_accum =
  let f st (t, x) =
        let (b, e) = begin_end_partition x
            st' = foldl begin_end_track st x
        in (st', (t, (b, e, st \\ e)))
  in snd . mapAccumL f []

{- | Variant that initially transforms 'Wseq' into non-overlapping begin-end sequence.
If the sequence was edited for overlaps this is indicated.
-}
wseq_begin_end_accum :: (Eq e, Ord t, Num t) => Wseq t e -> (Bool, Tseq t ([e], [e], [e]))
wseq_begin_end_accum sq =
  let ol = wseq_has_overlaps (==) sq
      sq_edit = if ol then wseq_remove_overlaps_rw (==) id sq else sq
      a_sq = tseq_begin_end_accum (tseq_group (wseq_begin_end sq_edit))
  in (ol, a_sq)

tseq_accumulate :: Eq a => Tseq t [Begin_End a] -> Tseq t [a]
tseq_accumulate =
  let f st (t, e) =
        let g st' = (st', (t, st'))
        in g (foldl begin_end_track st e)
  in snd . mapAccumL f []

{- | The transition sequence of /active/ elements.

>>> wseq_accumulate [((0,3),'a'),((1,2),'b'),((2,1),'c'),((3,3),'d')]
[(0,"a"),(1,"ba"),(2,"cba"),(3,"d"),(6,"")]
-}
wseq_accumulate :: (Eq a, Ord t, Num t) => Wseq t a -> Tseq t [a]
wseq_accumulate = tseq_accumulate . tseq_group . wseq_begin_end

{- | Inverse of 'wseq_begin_end' given a predicate function for locating the /end/ node of a /begin/ node.

>>> tseq_begin_end_to_wseq (==) [(0,Begin 'a'),(2,Begin 'b'),(4,End 'b'),(5,End 'a')]
[((0,5),'a'),((2,2),'b')]
-}
tseq_begin_end_to_wseq :: Num t => (a -> a -> Bool) -> Tseq t (Begin_End a) -> Wseq t a
tseq_begin_end_to_wseq cmp =
  let cmp' x e =
        case e of
          End x' -> cmp x x'
          _ -> False
      f e r = case seq_find (cmp' e) r of
        Nothing -> error "tseq_begin_end_to_wseq: no matching end?"
        Just (t, _) -> t
      go sq = case sq of
        [] -> []
        (_, End _) : sq' -> go sq'
        (t, Begin e) : sq' -> let t' = f e sq' in ((t, t' - t), e) : go sq'
  in go

-- * Interop

useq_to_dseq :: Useq t a -> Dseq t a
useq_to_dseq (t, e) = zip (repeat t) e

useq_to_wseq :: Num t => t -> Useq t a -> Wseq t a
useq_to_wseq t0 = dseq_to_wseq t0 . useq_to_dseq

{- | The conversion requires a start time and a /nil/ value used as an /eof/ marker.
Productive given indefinite input sequence.

>>> let r = zip [0,1,3,6,8,9] "abcde|"
>>> dseq_to_tseq 0 '|' (zip [1,2,3,2,1] "abcde") == r
True

>>> let d = zip [1,2,3,2,1] "abcde"
>>> let r = zip [0,1,3,6,8,9,10] "abcdeab"
>>> take 7 (dseq_to_tseq 0 undefined (cycle d)) == r
True
-}
dseq_to_tseq :: Num t => t -> a -> Dseq t a -> Tseq t a
dseq_to_tseq t0 nil = List.rezip (List.dx_d t0) (List.snoc nil)

{- | Variant where the /nil/ value is taken from the last element of the sequence.

>>> let r = zip [0,1,3,6,8,9] "abcdee"
>>> dseq_to_tseq_last 0 (zip [1,2,3,2,1] "abcde") == r
True
-}
dseq_to_tseq_last :: Num t => t -> Dseq t a -> Tseq t a
dseq_to_tseq_last t0 sq = dseq_to_tseq t0 (snd (last sq)) sq

{- | Variant where the final duration is discarded.

>>> dseq_to_tseq_discard 0 (zip [1,2,3,2,1] "abcde") == zip [0,1,3,6,8] "abcde"
True
-}
dseq_to_tseq_discard :: Num t => t -> Dseq t a -> Tseq t a
dseq_to_tseq_discard t0 = List.drop_last . dseq_to_tseq t0 undefined

{- | 'Iseq' to 'Tseq', requires t0.

>>> let r = zip [1,3,6,8,9] "abcde"
>>> iseq_to_tseq 0 (zip [1,2,3,2,1] "abcde") == r
True
-}
iseq_to_tseq :: Num t => t -> Iseq t a -> Tseq t a
iseq_to_tseq t0 = List.rezip (List.tail_err . List.dx_d t0) id

{- | The conversion requires a start time and does not consult the /logical/ duration.

>>> let p = pseq_zip (repeat undefined) (cycle [1,2]) (cycle [1,1,2]) "abcdef"
>>> pseq_to_wseq 0 p == wseq_zip [0,1,2,4,5,6] (cycle [1,2]) "abcdef"
True
-}
pseq_to_wseq :: Num t => t -> Pseq t a -> Wseq t a
pseq_to_wseq t0 sq =
  let (p, a) = unzip sq
      (_, d, f) = unzip3 p
      t = List.dx_d t0 f
  in wseq_zip t d a

{- | The last element of 'Tseq' is required to be an /eof/ marker that has no duration and is not represented in the 'Dseq'.
A 'nil' value is required in case the 'Tseq' does not begin at @0@.

>>> let r = zip [1,2,3,2,1] "abcde"
>>> tseq_to_dseq undefined (zip [0,1,3,6,8,9] "abcde|") == r
True

>>> let r = zip [1,2,3,2,1] "-abcd"
>>> tseq_to_dseq '-' (zip [1,3,6,8,9] "abcd|") == r
True
-}
tseq_to_dseq :: (Ord t, Num t) => a -> Tseq t a -> Dseq t a
tseq_to_dseq empty sq =
  let (t, a) = unzip sq
      d = List.d_dx t
  in case t of
      [] -> []
      t0 : _ -> if t0 > 0 then (t0, empty) : zip d a else zip d a

{- | Variant that requires a final duration be provided, and that the Tseq have no end marker.

>>> let r = zip [1,2,3,2,9] "abcde"
>>> tseq_to_dseq_final_dur undefined 9 (zip [0,1,3,6,8] "abcde") == r
True
-}
tseq_to_dseq_final_dur :: (Ord t, Num t) => a -> t -> Tseq t a -> Dseq t a
tseq_to_dseq_final_dur empty dur sq =
  let (t, a) = unzip sq
      d = List.d_dx t ++ [dur]
  in case t of
      [] -> []
      t0 : _ -> if t0 > 0 then (t0, empty) : zip d a else zip d a

{- | Variant that requires a total duration be provided, and that the Tseq have no end marker.

>>> let r = zip [1,2,3,2,7] "abcde"
>>> tseq_to_dseq_total_dur undefined 15 (zip [0,1,3,6,8] "abcde") == r
True
-}
tseq_to_dseq_total_dur :: (Ord t, Num t) => a -> t -> Tseq t a -> Dseq t a
tseq_to_dseq_total_dur empty dur sq = tseq_to_dseq_final_dur empty (dur - tseq_end sq) sq

{- | The last element of 'Tseq' is required to be an /eof/ marker that has no duration and is not represented in the 'Wseq'.
The duration of each value is either derived from the value, if a /dur/ function is given, or else the inter-offset time.

>>> let r = wseq_zip [0,1,3,6,8] [1,2,3,2,1] "abcde"
>>> tseq_to_wseq Nothing (zip [0,1,3,6,8,9] "abcde|") == r
True

>>> let r = wseq_zip [0,1,3,6,8] (map fromEnum "abcde") "abcde"
>>> tseq_to_wseq (Just fromEnum) (zip [0,1,3,6,8,9] "abcde|") == r
True
-}
tseq_to_wseq :: Num t => Maybe (a -> t) -> Tseq t a -> Wseq t a
tseq_to_wseq dur_f sq =
  let (t, a) = unzip sq
      d = case dur_f of
        Just f -> map f (fst (List.separate_last a))
        Nothing -> List.d_dx t
  in wseq_zip t d a

{- | Translate Tseq to Wseq using inter-offset times, up to indicated total duration, as element durations.

>>> tseq_to_wseq_iot 11 (zip [0,1,3,6,8] "abcde")
[((0,1),'a'),((1,2),'b'),((3,3),'c'),((6,2),'d'),((8,3),'e')]
-}
tseq_to_wseq_iot :: Num t => t -> Tseq t a -> Wseq t a
tseq_to_wseq_iot total_dur sq =
  let (t, e) = unzip sq
      d = zipWith (-) (List.tail_err t ++ [total_dur]) t
  in zip (zip t d) e

{- | Tseq to Iseq.

>>> tseq_to_iseq (zip [0,1,3,6,8,9] "abcde|") == zip [0,1,2,3,2,1] "abcde|"
True
-}
tseq_to_iseq :: Num t => Tseq t a -> Iseq t a
tseq_to_iseq =
  let recur n p =
        case p of
          [] -> []
          (t, e) : p' -> (t - n, e) : recur t p'
  in recur 0

{- | Requires start time.

>>> let r = zip (zip [0,1,3,6,8,9] [1,2,3,2,1]) "abcde"
>>> dseq_to_wseq 0 (zip [1,2,3,2,1] "abcde") == r
True
-}
dseq_to_wseq :: Num t => t -> Dseq t a -> Wseq t a
dseq_to_wseq t0 sq =
  let (d, a) = unzip sq
      t = List.dx_d t0 d
  in zip (zip t d) a

{- | Inverse of 'dseq_to_wseq'.  The /empty/ value is used to fill holes in 'Wseq'.
If values overlap at 'Wseq' durations are truncated.

>>> let w = wseq_zip [0,1,3,6,8,9] [1,2,3,2,1] "abcde"
>>> wseq_to_dseq '-' w == zip [1,2,3,2,1] "abcde"
True

>>> let w = wseq_zip [3,10] [6,2] "ab"
>>> wseq_to_dseq '-' w == zip [3,6,1,2] "-a-b"
True

>>> let w = wseq_zip [0,1] [2,2] "ab"
>>> wseq_to_dseq '-' w == zip [1,2] "ab"
True

>>> let w = wseq_zip [0,0,0] [2,2,2] "abc"
>>> wseq_to_dseq '-' w == zip [0,0,2] "abc"
True
-}
wseq_to_dseq :: (Num t, Ord t) => a -> Wseq t a -> Dseq t a
wseq_to_dseq empty sq =
  let f (((st0, d), e), ((st1, _), _)) =
        let d' = st1 - st0
        in case compare d d' of
            LT -> [(d, e), (d' - d, empty)]
            EQ -> [(d, e)]
            GT -> [(d', e)]
      ((_, dN), eN) = last sq
      r = concatMap f (List.adj2 1 sq) ++ [(dN, eN)]
  in case sq of
      ((st, _), _) : _ -> if st > 0 then (st, empty) : r else r
      [] -> error "wseq_to_dseq"

eseq_to_wseq :: Eseq t a -> Wseq t a
eseq_to_wseq = let f ((t, d, _), e) = ((t, d), e) in map f

-- * Measures

{- | Given a list of 'Dseq' (measures) convert to a list of 'Tseq' and the end time of the overall sequence.

>>> dseql_to_tseql 0 [zip [1,2,1] "abc",zip [3,2,1] "def"]
(10,[[(0,'a'),(1,'b'),(3,'c')],[(4,'d'),(7,'e'),(9,'f')]])
-}
dseql_to_tseql :: Num t => t -> [Dseq t a] -> (t, [Tseq t a])
dseql_to_tseql =
  let f z dv =
        let (tm, el) = unzip dv
            (z', r) = List.dx_d' z tm
        in (z', zip r el)
  in mapAccumL f

-- * Cycle

-- | List of cycles of 'Wseq'.
wseq_cycle_ls :: Num t => Wseq t a -> [Wseq t a]
wseq_cycle_ls sq =
  let (_, et) = wseq_tspan sq
      t_sq = iterate (+ et) 0
  in map (\x -> wseq_tmap (first (+ x)) sq) t_sq

{- | Only finite 'Wseq' can be cycled, the resulting Wseq is infinite.

>>> take 5 (wseq_cycle [((0,1),'a'),((3,3),'b')])
[((0,1),'a'),((3,3),'b'),((6,1),'a'),((9,3),'b'),((12,1),'a')]
-}
wseq_cycle :: Num t => Wseq t a -> Wseq t a
wseq_cycle = concat . wseq_cycle_ls

{- | Variant cycling only /n/ times.

>>> wseq_cycle_n 3 [((0,1),'a'),((3,3),'b')]
[((0,1),'a'),((3,3),'b'),((6,1),'a'),((9,3),'b'),((12,1),'a'),((15,3),'b')]
-}
wseq_cycle_n :: Num t => Int -> Wseq t a -> Wseq t a
wseq_cycle_n n = concat . take n . wseq_cycle_ls

-- | 'wseq_until' of 'wseq_cycle'.
wseq_cycle_until :: (Num t, Ord t) => t -> Wseq t a -> Wseq t a
wseq_cycle_until et = wseq_until et . wseq_cycle

-- * Type specialised maps

dseq_tmap :: (t -> t') -> Dseq t a -> Dseq t' a
dseq_tmap = seq_tmap

pseq_tmap :: ((t, t, t) -> (t', t', t')) -> Pseq t a -> Pseq t' a
pseq_tmap = seq_tmap

tseq_tmap :: (t -> t') -> Dseq t a -> Dseq t' a
tseq_tmap = seq_tmap

tseq_bimap :: (t -> t') -> (e -> e') -> Tseq t e -> Tseq t' e'
tseq_bimap = seq_bimap

wseq_tmap :: ((t, t) -> (t', t')) -> Wseq t a -> Wseq t' a
wseq_tmap = seq_tmap

dseq_map :: (a -> b) -> Dseq t a -> Dseq t b
dseq_map = seq_map

pseq_map :: (a -> b) -> Pseq t a -> Pseq t b
pseq_map = seq_map

tseq_map :: (a -> b) -> Tseq t a -> Tseq t b
tseq_map = seq_map

wseq_map :: (a -> b) -> Wseq t a -> Wseq t b
wseq_map = seq_map

-- * Type specialised filter

dseq_tfilter :: (t -> Bool) -> Dseq t a -> Dseq t a
dseq_tfilter = seq_tfilter

iseq_tfilter :: (t -> Bool) -> Iseq t a -> Iseq t a
iseq_tfilter = seq_tfilter

pseq_tfilter :: ((t, t, t) -> Bool) -> Pseq t a -> Pseq t a
pseq_tfilter = seq_tfilter

tseq_tfilter :: (t -> Bool) -> Tseq t a -> Tseq t a
tseq_tfilter = seq_tfilter

wseq_tfilter :: ((t, t) -> Bool) -> Wseq t a -> Wseq t a
wseq_tfilter = seq_tfilter

dseq_filter :: (a -> Bool) -> Dseq t a -> Dseq t a
dseq_filter = seq_filter

iseq_filter :: (a -> Bool) -> Iseq t a -> Iseq t a
iseq_filter = seq_filter

pseq_filter :: (a -> Bool) -> Pseq t a -> Pseq t a
pseq_filter = seq_filter

tseq_filter :: (a -> Bool) -> Tseq t a -> Tseq t a
tseq_filter = seq_filter

wseq_filter :: (a -> Bool) -> Wseq t a -> Wseq t a
wseq_filter = seq_filter

-- * Type specialised maybe

wseq_map_maybe :: (a -> Maybe b) -> Wseq t a -> Wseq t b
wseq_map_maybe = seq_map_maybe

wseq_cat_maybes :: Wseq t (Maybe a) -> Wseq t a
wseq_cat_maybes = seq_cat_maybes

-- * Maps

{- | Requires but does not check that there are no duplicate time points in Tseq.

>>> tseq_to_map [(0, 'a'), (0, 'b')] == tseq_to_map [(0, 'b')]
True
-}
tseq_to_map :: Ord t => Tseq t e -> Map.Map t e
tseq_to_map = Map.fromList
