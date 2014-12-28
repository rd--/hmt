-- | Basic temporal sequence functions.
module Music.Theory.Time.Seq where

import Data.Function {- base -}
import Data.List {- base -}
import qualified Data.List.Ordered as O {- data-ordlist -}
import qualified Data.Map as M {- containers -}
import Data.Maybe {- base -}
import Data.Monoid {- base -}
import Data.Ratio {- base -}
import Safe {- safe -}

import Music.Theory.Function {- hmt -}
import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Math as T {- hmt -}
import qualified Music.Theory.Tuple as T {- hmt -}

-- * Types

-- | Sequence of elements with uniform duration.
type Useq t a = (t,[a])

-- | Duration sequence.  The duration is the /forward/ duration of the
-- value, if it has other durations they must be encoded at /a/.
type Dseq t a = [(t,a)]

-- | Inter-offset sequence.  The duration is the interval /before/ the
-- value.  To indicate the duration of the final value /a/ must have
-- an /nil/ (end of sequence) value.
type Iseq t a = [(t,a)]

-- | Pattern sequence.  The duration is a triple of /logical/,
-- /sounding/ and /forward/ durations.
type Pseq t a = [((t,t,t),a)]

-- | Time-point sequence.  To express holes /a/ must have a /empty/
-- value.  To indicate the duration of the final value /a/ must have
-- an /nil/ (end of sequence) value.
type Tseq t a = [(t,a)]

-- | Window sequence.  The temporal field is (/time/,/duration/).
-- Holes exist where @t(n) + d(n)@ '<' @t(n+1)@.  Overlaps exist where
-- the same relation is '>'.
type Wseq t a = [((t,t),a)]

-- * Zip

pseq_zip :: [t] -> [t] -> [t] -> [a] -> Pseq t a
pseq_zip l o f a = (zip (zip3 l o f) a)

wseq_zip :: [t] -> [t] -> [a] -> Wseq t a
wseq_zip t d a = (zip (zip t d) a)

-- * Time span

-- | Given functions for deriving start and end times calculate time
-- span of sequence.
--
-- > seq_tspan id id [] == (0,0)
-- > seq_tspan id id (zip [0..9] ['a'..]) == (0,9)
seq_tspan :: Num n => (t -> n) -> (t -> n) -> [(t,a)] -> (n,n)
seq_tspan st et sq =
    (maybe 0 (st . fst) (headMay sq)
    ,maybe 0 (et . fst) (lastMay sq))

tseq_tspan :: Num t => Tseq t a -> (t,t)
tseq_tspan = seq_tspan id id

wseq_tspan :: Num t => Wseq t a -> (t,t)
wseq_tspan = seq_tspan fst (uncurry (+))

-- * Duration

dseq_dur :: Num t => Dseq t a -> t
dseq_dur = sum . map fst

iseq_dur :: Num t => Iseq t a -> t
iseq_dur = sum . map fst

pseq_dur :: Num t => Pseq t a -> t
pseq_dur = sum . map (T.t3_third . fst)

-- | The interval of 'tseq_tspan'.
--
-- > tseq_dur (zip [0..] "abcde|") == 5
tseq_dur :: Num t => Tseq t a -> t
tseq_dur = uncurry subtract . tseq_tspan

-- | The interval of 'wseq_tspan'.
--
-- > wseq_dur (zip (zip [0..] (repeat 2)) "abcde") == 6
wseq_dur :: Num t => Wseq t a -> t
wseq_dur = uncurry subtract . wseq_tspan

-- * Window

-- | Keep only elements in the indicated temporal window.
--
-- > let r = [((5,1),'e'),((6,1),'f'),((7,1),'g'),((8,1),'h')]
-- > in wseq_twindow (5,9) (zip (zip [1..10] (repeat 1)) ['a'..]) == r
wseq_twindow :: (Num t, Ord t) => (t,t) -> Wseq t a -> Wseq t a
wseq_twindow (w0,w1) =
    let f (st,du) = w0 <= st && (st + du) <= w1
    in wseq_tfilter f

-- * Append

dseq_append :: Dseq t a -> Dseq t a -> Dseq t a
dseq_append = (++)

iseq_append :: Iseq t a -> Iseq t a -> Iseq t a
iseq_append = (++)

pseq_append :: Pseq t a -> Pseq t a -> Pseq t a
pseq_append = (++)

-- * Merge

-- | Merge comparing only on time.
tseq_merge :: Ord t => Tseq t a -> Tseq t a -> Tseq t a
tseq_merge = O.mergeBy (compare `on` fst)

-- | Merge, where times are equal compare values.
tseq_merge_by :: Ord t => T.Compare_F a -> Tseq t a -> Tseq t a -> Tseq t a
tseq_merge_by cmp = T.merge_by_two_stage fst cmp snd

{- | Merge, where times are equal apply /f/ to form a single value.

> let {p = zip [1,3,5] "abc"
>     ;q = zip [1,2,3] "ABC"
>     ;left_r = [(1,'a'),(2,'B'),(3,'b'),(5,'c')]
>     ;right_r = [(1,'A'),(2,'B'),(3,'C'),(5,'c')]}
> in tseq_merge_resolve (\x _ -> x) p q == left_r &&
>    tseq_merge_resolve (\_ x -> x) p q == right_r

-}
tseq_merge_resolve :: Ord t => (a -> a -> a) -> Tseq t a -> Tseq t a -> Tseq t a
tseq_merge_resolve f =
    let cmp = compare `on` fst
        g (t,p) (_,q) = (t,f p q)
    in T.merge_by_resolve g cmp

wseq_merge :: Ord t => Wseq t a -> Wseq t a -> Wseq t a
wseq_merge = O.mergeBy (compare `on` (fst . fst))

-- * Lookup

-- | Locate nodes to the left and right of indicated time.
tseq_lookup_window_by :: (t -> t -> Ordering) -> Tseq t e -> t -> (Maybe (t,e),Maybe (t,e))
tseq_lookup_window_by cmp =
    let recur l sq t =
            case sq of
              [] -> (l,Nothing)
              (t',e):sq' -> case cmp t t' of
                              LT -> (l,Just (t',e))
                              _ -> case sq' of
                                     [] -> (Just (t',e),Nothing)
                                     (t'',e'):_ -> case cmp t t'' of
                                                     LT -> (Just (t',e),Just (t'',e'))
                                                     _ -> recur (Just (t',e)) sq' t
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

data Interpolation_T = None | Linear
                     deriving (Eq,Enum,Show)

-- | Variant of 'Tseq' where nodes have an 'Intepolation_T' value.
type Lseq t a = Tseq (t,Interpolation_T) a

-- | Linear interpolation.
lerp :: (Fractional t,Real t,Fractional e) => (t,e) -> (t,e) -> t -> e
lerp (t0,e0) (t1,e1) t =
    let n = t1 - t0
        m = t - t0
        l = m / n
    in realToFrac l * (e1 - e0) + e0

-- | Temporal map.
lseq_tmap :: (t -> t') -> Lseq t a -> Lseq t' a
lseq_tmap f = let g ((t,i),e) = ((f t,i),e) in map g

-- | This can give 'Nothing' if /t/ precedes the 'Lseq' or if /t/ is
-- after the final element of 'Lseq' and that element has an
-- interpolation type other than 'None'.
lseq_lookup :: (Fractional t,Real t,Fractional e) => (t -> t -> Ordering) -> Lseq t e -> t -> Maybe e
lseq_lookup cmp sq t =
    case tseq_lookup_window_by (cmp `on` fst) sq (t,undefined) of
      (Nothing,_) -> Nothing
      (Just ((_,None),e),_) -> Just e
      (Just ((t0,Linear),e0),Just ((t1,_),e1)) -> Just (lerp (t0,e0) (t1,e1) t)
      _ -> Nothing

-- | 'error'ing variant.
lseq_lookup_err :: (Fractional t,Real t,Fractional e) => (t -> t -> Ordering) -> Lseq t e -> t -> e
lseq_lookup_err cmp sq = fromMaybe (error "lseq_lookup") . lseq_lookup cmp sq

-- * Map, Filter, Find

seq_tmap :: (t -> t') -> [(t,a)] -> [(t',a)]
seq_tmap f = map (\(p,q) -> (f p,q))

seq_map :: (b -> c) -> [(a,b)] -> [(a,c)]
seq_map f = map (\(p,q) -> (p,f q))

-- | Map /t/ and /e/ simultaneously.
seq_bimap :: (t -> t') -> (e -> e') -> [(t,e)] -> [(t',e')]
seq_bimap f g = map (\(p,q) -> (f p,g q))

seq_tfilter :: (t -> Bool) -> [(t,a)] -> [(t,a)]
seq_tfilter f = filter (f . fst)

seq_filter :: (b -> Bool) -> [(a,b)] -> [(a,b)]
seq_filter f = filter (f . snd)

seq_find :: (a -> Bool) -> [(t,a)] -> Maybe (t,a)
seq_find f = let f' (_,a) = f a in find f'

-- * Maybe

-- | 'mapMaybe' variant.
seq_map_maybe :: (p -> Maybe q) -> [(t,p)] -> [(t,q)]
seq_map_maybe f =
    let g (t,e) = maybe Nothing (\e' -> Just (t,e')) (f e)
    in mapMaybe g

-- | Variant of 'catMaybes'.
seq_cat_maybes :: [(t,Maybe q)] -> [(t,q)]
seq_cat_maybes = seq_map_maybe id

-- | If value is unchanged, according to /f/, replace with 'Nothing'.
--
-- > let r = [(1,'s'),(2,'t'),(4,'r'),(6,'i'),(7,'n'),(9,'g')]
-- > in seq_cat_maybes (seq_changed_by (==) (zip [1..] "sttrrinng")) == r
seq_changed_by :: (a -> a -> Bool) -> [(t,a)] -> [(t,Maybe a)]
seq_changed_by f l =
    let recur z sq =
            case sq of
              [] -> []
              (t,e):sq' -> if f e z
                           then (t,Nothing) : recur z sq'
                           else (t,Just e) : recur e sq'
    in case l of
         [] -> []
         (t,e) : l' -> (t,Just e) : recur e l'

-- | 'seq_changed_by' '=='.
seq_changed :: Eq a => [(t,a)] -> [(t,Maybe a)]
seq_changed = seq_changed_by (==)

-- * Specialised temporal maps.

-- | Apply /f/ at time points of 'Wseq'.
wseq_tmap_st :: (t -> t) -> Wseq t a -> Wseq t a
wseq_tmap_st f = let g (t,d) = (f t,d) in seq_tmap g

-- | Apply /f/ at durations of elements of 'Wseq'.
wseq_tmap_dur :: (t -> t) -> Wseq t a -> Wseq t a
wseq_tmap_dur f = let g (t,d) = (t,f d) in seq_tmap g

-- * Partition

-- | Given a function that determines a /voice/ for a value, partition
-- a sequence into voices.
seq_partition :: Ord v => (a -> v) -> [(t,a)] -> [(v,[(t,a)])]
seq_partition voice sq =
    let assign m (t,a) = M.insertWith (++) (voice a) [(t,a)] m
        from_map = sortBy (compare `on` fst) .
                   map (\(v,l) -> (v,reverse l)) .
                   M.toList
    in from_map (foldl assign M.empty sq)

-- | Type specialised 'seq_partition'.
--
-- > let {p = zip [0,1,3,5] (zip (repeat 0) "abcd")
-- >     ;q = zip [2,4,6,7] (zip (repeat 1) "ABCD")
-- >     ;sq = tseq_merge p q}
-- > in tseq_partition fst sq == [(0,p),(1,q)]
tseq_partition :: Ord v => (a -> v) -> Tseq t a -> [(v,Tseq t a)]
tseq_partition = seq_partition

wseq_partition :: Ord v => (a -> v) -> Wseq t a -> [(v,Wseq t a)]
wseq_partition = seq_partition

-- * Coalesce

-- | Given a decision predicate and a join function, recursively join
-- adjacent elements.
--
-- > coalesce_f undefined undefined [] == []
-- > coalesce_f (==) const "abbcccbba" == "abcba"
-- > coalesce_f (==) (+) [1,2,2,3,3,3] == [1,4,6,3]
coalesce_f :: (t -> t -> Bool) -> (t -> t -> t) -> [t] -> [t]
coalesce_f dec_f jn_f z =
    let recur p l =
            case l of
              [] -> [p]
              c:l' -> if dec_f p c
                      then recur (jn_f p c) l'
                      else p : recur c l'
    in case z of
         [] -> []
         e0:z' -> recur e0 z'

-- | 'coalesce_f' using 'mappend' for the join function.
coalesce_m :: Monoid t => (t -> t -> Bool) -> [t] -> [t]
coalesce_m dec_f = coalesce_f dec_f mappend

-- | Form of 'coalesce_f' where the decision predicate is on the
-- /element/, and a join function sums the /times/.
--
-- > let r = [(1,'a'),(2,'b'),(3,'c'),(2,'d'),(1,'e')]
-- > in seq_coalesce (==) const (useq_to_dseq (1,"abbcccdde")) == r
seq_coalesce :: Num t => (a -> a -> Bool) -> (a -> a -> a) -> [(t,a)] -> [(t,a)]
seq_coalesce dec_f jn_f =
    let dec_f' = dec_f `on` snd
        jn_f' (t1,a1) (t2,a2) = (t1 + t2,jn_f a1 a2)
    in coalesce_f dec_f' jn_f'

dseq_coalesce :: Num t => (a -> a -> Bool) -> (a -> a -> a) -> Dseq t a -> Dseq t a
dseq_coalesce = seq_coalesce

-- | Given /equality/ predicate, simplify sequence by summing
-- durations of adjacent /equal/ elements.  This is a special case of
-- 'dseq_coalesce' where the /join/ function is 'const'.  The
-- implementation is simpler and non-recursive.
--
-- > let {d = useq_to_dseq (1,"abbcccdde")
-- >     ;r = dseq_coalesce (==) const d}
-- > in dseq_coalesce' (==) d == r
dseq_coalesce' :: Num t => (a -> a -> Bool) -> Dseq t a -> Dseq t a
dseq_coalesce' eq =
    let f l = let (t,e:_) = unzip l in (sum t,e)
    in map f . groupBy (eq `on` snd)

iseq_coalesce :: Num t => (a -> a -> Bool) -> (a -> a -> a) -> Iseq t a -> Iseq t a
iseq_coalesce = seq_coalesce

-- * T-coalesce

seq_tcoalesce :: (t -> t -> Bool) -> (a -> a -> a) -> [(t,a)] -> [(t,a)]
seq_tcoalesce eq_f jn_f =
    let dec_f = eq_f `on` fst
        jn_f' (t,a1) (_,a2) = (t,jn_f a1 a2)
    in coalesce_f dec_f jn_f'

tseq_tcoalesce :: Eq t => (a -> a -> a) -> Tseq t a -> Tseq t a
tseq_tcoalesce = seq_tcoalesce (==)

wseq_tcoalesce :: ((t,t) -> (t,t) -> Bool) -> (a -> a -> a) -> Wseq t a -> Wseq t a
wseq_tcoalesce = seq_tcoalesce

-- * Group

-- | Post-process 'groupBy' of /cmp/ 'on' 'fst'.
--
-- > let r = [(0,"a"),(1,"bc"),(2,"de"),(3,"f")]
-- > in group_f (==) (zip [0,1,1,2,2,3] ['a'..]) == r
group_f :: (Eq t,Num t) => (t -> t -> Bool) -> [(t,a)] -> [(t,[a])]
group_f cmp =
    let f l = let (t,a) = unzip l
              in case t of
                   [] -> error "group_f: []?"
                   t0:_ -> (t0,a)
    in map f . groupBy (cmp `on` fst)

-- | Group values at equal time points.
--
-- > let r = [(0,"a"),(1,"bc"),(2,"de"),(3,"f")]
-- > in tseq_group (zip [0,1,1,2,2,3] ['a'..]) == r
tseq_group :: (Eq t,Num t) => Tseq t a -> Tseq t [a]
tseq_group = group_f (==)

-- | Group values where the inter-offset time is @0@ to the left.
--
-- > let r = [(0,"a"),(1,"bcd"),(1,"ef")]
-- > in iseq_group (zip [0,1,0,0,1,0] ['a'..]) == r
iseq_group :: (Eq t,Num t) => Iseq t a -> Iseq t [a]
iseq_group = group_f (\_ d -> d == 0)

-- * Fill

-- | Set durations so that there are no gaps or overlaps.
--
-- > let r = wseq_zip [0,3,5] [3,2,1] "abc"
-- > in wseq_fill_dur (wseq_zip [0,3,5] [2,1,1] "abc") == r
wseq_fill_dur :: Num t => Wseq t a -> Wseq t a
wseq_fill_dur l =
    let f (((t1,_),e),((t2,_),_)) = ((t1,t2-t1),e)
    in map f (T.adj2 1 l) ++ [last l]

-- * Dseq

dseq_lcm :: Dseq Rational e -> Integer
dseq_lcm = foldl1 lcm . map (denominator . fst)

-- | Scale by lcm so that all durations are integral.
dseq_set_whole :: [Dseq Rational e] -> [Dseq Integer e]
dseq_set_whole sq =
    let m = maximum (map dseq_lcm sq)
        t_f n = T.rational_whole_err (n * fromIntegral m)
    in map (dseq_tmap t_f) sq

-- * Tseq

-- | Given a a default value, a 'Tseq' /sq/ and a list of time-points
-- /t/, generate a Tseq that is a union of the timepoints at /sq/ and
-- /t/ where times in /t/ not at /sq/ are given the /current/ value,
-- or /def/ if there is no value.
--
-- > tseq_latch 'a' [(2,'b'),(4,'c')] [1..5] == zip [1..5] "abbcc"
tseq_latch :: Ord t => a -> Tseq t a -> [t] -> Tseq t a
tseq_latch def sq t =
    case (sq,t) of
      ([],_) -> zip t (repeat def)
      (_,[]) -> []
      ((sq_t,sq_e):sq',t0:t') -> case compare sq_t t0 of
                                   LT -> (sq_t,sq_e) : tseq_latch sq_e sq' t
                                   EQ -> (sq_t,sq_e) : tseq_latch sq_e sq' t'
                                   GT -> (t0,def) : tseq_latch def sq t'

-- * Wseq

-- | Transform 'Wseq' to 'Tseq' by discaring durations.
wseq_discard_dur :: Wseq t a -> Tseq t a
wseq_discard_dur = let f ((t,_),e) = (t,e) in map f

-- | Edit durations to ensure that notes don't overlap.  If the same
-- note is played simultaneously delete shorter note.  If a note
-- extends into a later note shorten duration (apply /d_fn/ to iot).
wseq_remove_overlaps :: (Eq e,Ord t,Num t) =>
                        (e -> e -> Bool) -> (t -> t) ->
                        Wseq t e -> Wseq t e
wseq_remove_overlaps eq_fn d_fn =
    let go sq =
            case sq of
              [] -> []
              ((t,d),a):sq' ->
                  case find (eq_fn a . snd) sq' of
                      Nothing -> ((t,d),a) : go sq'
                      Just ((t',d'),a') ->
                          if t == t'
                          then if d <= d'
                               then -- delete LHS
                                   go sq'
                               else -- delete RHS
                                   ((t,d),a) :
                                   go (delete ((t',d'),a') sq')
                          else if t' < t + d
                               then ((t,d_fn (t' - t)),a) : go sq'
                               else ((t,d),a) : go sq'
    in go

-- | Unjoin elements (assign equal time stamps to all elements).
seq_unjoin :: [(t,[e])] -> [(t,e)]
seq_unjoin = let f (t,e) = zip (repeat t) e in concatMap f

-- | Type specialised.
wseq_unjoin :: Wseq t [e] -> Wseq t e
wseq_unjoin = seq_unjoin

-- * On/Off

-- | Container for values that have /on/ and /off/ modes.
data On_Off a = On a | Off a deriving (Eq,Show)

-- | Structural comparison at 'On_Off', 'On' compares less than 'Off'.
cmp_on_off :: On_Off a -> On_Off b -> Ordering
cmp_on_off p q =
    case (p,q) of
      (On _,Off _) -> LT
      (On _,On _) -> EQ
      (Off _,Off _) -> EQ
      (Off _,On _) -> GT

-- | Translate container types.
either_to_on_off :: Either a a -> On_Off a
either_to_on_off p =
    case p of
      Left a -> On a
      Right a -> Off a

-- | Translate container types.
on_off_to_either :: On_Off a -> Either a a
on_off_to_either p =
    case p of
      On a -> Left a
      Off a -> Right a

-- | Convert 'Wseq' to 'Tseq' transforming elements to 'On' and 'Off'
-- parts.  When merging, /off/ elements precede /on/ elements at equal
-- times.
--
-- > let {sq = [((0,5),'a'),((2,2),'b')]
-- >     ;r = [(0,On 'a'),(2,On 'b'),(4,Off 'b'),(5,Off 'a')]}
-- > in wseq_on_off sq == r
--
-- > let {sq = [((0,1),'a'),((1,1),'b'),((2,1),'c')]
-- >     ;r = [(0,On 'a'),(1,Off 'a')
-- >          ,(1,On 'b'),(2,Off 'b')
-- >          ,(2,On 'c'),(3,Off 'c')]}
-- > in wseq_on_off sq == r
wseq_on_off :: (Num t, Ord t) => Wseq t a -> Tseq t (On_Off a)
wseq_on_off sq =
    let f ((t,d),a) = [(t,On a),(t + d,Off a)]
        g l =
            case l of
              [] -> []
              e:l' -> tseq_merge_by (T.ordering_invert .: cmp_on_off) e (g l')
    in g (map f sq)

-- | 'on_off_to_either' of 'wseq_on_off'.
wseq_on_off_either :: (Num t, Ord t) => Wseq t a -> Tseq t (Either a a)
wseq_on_off_either = tseq_map on_off_to_either . wseq_on_off

-- | Variant that applies /on/ and /off/ functions to nodes.
--
-- > let {sq = [((0,5),'a'),((2,2),'b')]
-- >     ;r = [(0,'A'),(2,'B'),(4,'b'),(5,'a')]}
-- > in wseq_on_off_f Data.Char.toUpper id sq == r
wseq_on_off_f :: (Ord t,Num t) => (a -> b) -> (a -> b) -> Wseq t a -> Tseq t b
wseq_on_off_f f g = tseq_map (either f g) . wseq_on_off_either

-- | Inverse of 'wseq_on_off' given a predicate function for locating
-- the /off/ node of an /on/ node.
--
-- > let {sq = [(0,On 'a'),(2,On 'b'),(4,Off 'b'),(5,Off 'a')]
-- >     ;r = [((0,5),'a'),((2,2),'b')]}
-- > in tseq_on_off_to_wseq (==) sq == r
tseq_on_off_to_wseq :: Num t => (a -> a -> Bool) -> Tseq t (On_Off a) -> Wseq t a
tseq_on_off_to_wseq cmp =
    let cmp' x e =
            case e of
              Off x' -> cmp x x'
              _ -> False
        f e r = case seq_find (cmp' e) r of
                        Nothing -> error "tseq_on_off_to_wseq: no matching off?"
                        Just (t,_) -> t
        go sq = case sq of
                  [] -> []
                  (_,Off _) : sq' -> go sq'
                  (t,On e) : sq' -> let t' = f e sq' in ((t,t' - t),e) : go sq'
    in go

-- * Interop

useq_to_dseq :: Useq t a -> Dseq t a
useq_to_dseq (t,e) = zip (repeat t) e

-- | The conversion requires a start time and a /nil/ value used as an
-- /eof/ marker. Productive given indefinite input sequence.
--
-- > let r = zip [0,1,3,6,8,9] "abcde|"
-- > in dseq_to_tseq 0 '|' (zip [1,2,3,2,1] "abcde") == r
--
-- > let {d = zip [1,2,3,2,1] "abcde"
-- >     ;r = zip [0,1,3,6,8,9,10] "abcdeab"}
-- > in take 7 (dseq_to_tseq 0 undefined (cycle d)) == r
dseq_to_tseq :: Num t => t -> a -> Dseq t a -> Tseq t a
dseq_to_tseq t0 nil sq =
    let (d,a) = unzip sq
        t = T.dx_d t0 d
        a' = a ++ [nil]
    in zip t a'

-- | Variant where the /nil/ value is taken from the last element of
-- the sequence.
--
-- > let r = zip [0,1,3,6,8,9] "abcdee"
-- > in dseq_to_tseq_last 0 (zip [1,2,3,2,1] "abcde") == r
dseq_to_tseq_last :: Num t => t -> Dseq t a -> Tseq t a
dseq_to_tseq_last t0 sq = dseq_to_tseq t0 (snd (last sq)) sq

-- | The conversion requires a start time and does not consult the
-- /logical/ duration.
--
-- > let p = pseq_zip (repeat undefined) (cycle [1,2]) (cycle [1,1,2]) "abcdef"
-- > in pseq_to_wseq 0 p == wseq_zip [0,1,2,4,5,6] (cycle [1,2]) "abcdef"
pseq_to_wseq :: Num t => t -> Pseq t a -> Wseq t a
pseq_to_wseq t0 sq =
    let (p,a) = unzip sq
        (_,d,f) = unzip3 p
        t = T.dx_d t0 f
    in wseq_zip t d a

-- | The last element of 'Tseq' is required to be an /eof/ marker that
-- has no duration and is not represented in the 'Dseq'.
--
-- > let r = zip [1,2,3,2,1] "abcde"
-- > in tseq_to_dseq undefined (zip [0,1,3,6,8,9] "abcde|") == r
--
-- > let r = zip [1,2,3,2,1] "-abcd"
-- > in tseq_to_dseq '-' (zip [1,3,6,8,9] "abcd|") == r
tseq_to_dseq :: (Ord t,Num t) => a -> Tseq t a -> Dseq t a
tseq_to_dseq empty sq =
    let (t,a) = unzip sq
        d = T.d_dx t
    in case t of
         [] -> []
         t0:_ -> if t0 > 0 then (t0,empty) : zip d a else zip d a

-- | The last element of 'Tseq' is required to be an /eof/ marker that
-- has no duration and is not represented in the 'Wseq'.  The duration
-- of each value is either derived from the value, if an /dur/
-- function is given, or else the inter-offset time.
--
-- > let r = wseq_zip [0,1,3,6,8] [1,2,3,2,1] "abcde"
-- > in tseq_to_wseq Nothing (zip [0,1,3,6,8,9] "abcde|") == r
--
-- > let r = wseq_zip [0,1,3,6,8] (map fromEnum "abcde") "abcde"
-- > in tseq_to_wseq (Just fromEnum) (zip [0,1,3,6,8,9] "abcde|") == r
tseq_to_wseq :: Num t => Maybe (a -> t) -> Tseq t a -> Wseq t a
tseq_to_wseq dur_f sq =
    let (t,a) = unzip sq
        d = case dur_f of
              Just f -> map f (fst (T.separate_last a))
              Nothing -> T.d_dx t
    in wseq_zip t d a

tseq_to_iseq :: Num t => Tseq t a -> Dseq t a
tseq_to_iseq =
    let recur n p =
            case p of
              [] -> []
              (t,e):p' -> (t - n,e) : recur t p'
    in recur 0

-- | Requires start time.
--
-- > let r = zip (zip [0,1,3,6,8,9] [1,2,3,2,1]) "abcde"
-- > in dseq_to_wseq 0 (zip [1,2,3,2,1] "abcde") == r
dseq_to_wseq :: Num t => t -> Dseq t a -> Wseq t a
dseq_to_wseq t0 sq =
    let (d,a) = unzip sq
        t = T.dx_d t0 d
    in zip (zip t d) a

-- | Inverse of 'dseq_to_wseq'.  The /empty/ value is used to fill
-- holes in 'Wseq'.  If values overlap at 'Wseq' durations are
-- truncated.
--
-- > let w = wseq_zip [0,1,3,6,8,9] [1,2,3,2,1] "abcde"
-- > in wseq_to_dseq '-' w == zip [1,2,3,2,1] "abcde"
--
-- > let w = wseq_zip [3,10] [6,2] "ab"
-- > in wseq_to_dseq '-' w == zip [3,6,1,2] "-a-b"
--
-- > let w = wseq_zip [0,1] [2,2] "ab"
-- > in wseq_to_dseq '-' w == zip [1,2] "ab"
--
-- > let w = wseq_zip [0,0,0] [2,2,2] "abc"
-- > in wseq_to_dseq '-' w == zip [0,0,2] "abc"
wseq_to_dseq :: (Num t,Ord t) => a -> Wseq t a -> Dseq t a
wseq_to_dseq empty sq =
    let f (((st0,d),e),((st1,_),_)) =
            let d' = st1 - st0
            in case compare d d' of
                 LT -> [(d,e),(d'-d,empty)]
                 EQ -> [(d,e)]
                 GT -> [(d',e)]
        ((_,dN),eN) = last sq
        r = concatMap f (T.adj2 1 sq) ++ [(dN,eN)]
    in case sq of
         ((st,_),_):_ -> if st > 0 then (st,empty) : r else r
         [] -> error "wseq_to_dseq"

-- * Measures

-- | Given a list of 'Dseq' (measures) convert to a list of 'Tseq' and
-- the end time of the overall sequence.
--
-- > let r = [[(0,'a'),(1,'b'),(3,'c')],[(4,'d'),(7,'e'),(9,'f')]]
-- > in dseql_to_tseql 0 [zip [1,2,1] "abc",zip [3,2,1] "def"] == (10,r)
dseql_to_tseql :: Num t => t -> [Dseq t a] -> (t,[Tseq t a])
dseql_to_tseql =
    let f z dv =
            let (tm,el) = unzip dv
                (z',r) = T.dx_d' z tm
            in (z',zip r el)
    in mapAccumL f

-- * Type specialised map

dseq_tmap :: (t -> t') -> Dseq t a -> Dseq t' a
dseq_tmap = seq_tmap

pseq_tmap :: ((t,t,t) -> (t',t',t')) -> Pseq t a -> Pseq t' a
pseq_tmap = seq_tmap

tseq_tmap :: (t -> t') -> Dseq t a -> Dseq t' a
tseq_tmap = seq_tmap

tseq_bimap :: (t -> t') -> (e -> e') -> Tseq t e -> Tseq t' e'
tseq_bimap = seq_bimap

wseq_tmap :: ((t,t) -> (t',t')) -> Wseq t a -> Wseq t' a
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

pseq_tfilter :: ((t,t,t) -> Bool) -> Pseq t a -> Pseq t a
pseq_tfilter = seq_tfilter

tseq_tfilter :: (t -> Bool) -> Tseq t a -> Tseq t a
tseq_tfilter = seq_tfilter

wseq_tfilter :: ((t,t) -> Bool) -> Wseq t a -> Wseq t a
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
