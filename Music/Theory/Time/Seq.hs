-- | Basic temporal sequence functions.
module Music.Theory.Time.Seq where

import Data.Function {- base -}
import Data.List as L {- base -}
import qualified Data.List.Ordered as L {- data-ordlist -}
import qualified Data.Map as M {- containers -}
import Data.Monoid {- base -}
import Safe {- safe -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Tuple as T {- hmt -}

-- * Types

-- | Duration sequence.  The duration is the /logical/ duration of the
-- value.
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

-- | Window sequence.  Holes exist where @t(n) + d(n)@ '<' @t(n+1)@.
-- Overlaps exist where the same relation is '>'.
type Wseq t a = [((t,t),a)]

-- * Zip

wseq_zip :: [t] -> [t] -> [a] -> Wseq t a
wseq_zip t d a = (zip (zip t d) a)

-- * Time span

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

-- > tseq_dur (zip [0..9] ['a'..])
tseq_dur :: Num t => Tseq t a -> t
tseq_dur = uncurry (flip (-)) . tseq_tspan

-- > wseq_dur (zip (zip [0..9] (repeat 2)) ['a'..]) == 11
wseq_dur :: Num t => Wseq t a -> t
wseq_dur = uncurry (flip (-)) . wseq_tspan

-- * Append

dseq_append :: Dseq t a -> Dseq t a -> Dseq t a
dseq_append = (++)

iseq_append :: Iseq t a -> Iseq t a -> Iseq t a
iseq_append = (++)

pseq_append :: Pseq t a -> Pseq t a -> Pseq t a
pseq_append = (++)

-- * Merge

tseq_merge :: Ord t => Tseq t a -> Tseq t a -> Tseq t a
tseq_merge = L.mergeBy (compare `on` fst)

wseq_merge :: Ord t => Wseq t a -> Wseq t a -> Wseq t a
wseq_merge = L.mergeBy (compare `on` (fst . fst))

-- * Map, Filter, Find

seq_tmap :: (t -> t') -> [(t,a)] -> [(t',a)]
seq_tmap f = map (\(p,q) -> (f p,q))

seq_map :: (b -> c) -> [(a,b)] -> [(a,c)]
seq_map f = map (\(p,q) -> (p,f q))

seq_tfilter :: (t -> Bool) -> [(t,a)] -> [(t,a)]
seq_tfilter f = filter (f . fst)

seq_filter :: (b -> Bool) -> [(a,b)] -> [(a,b)]
seq_filter f = filter (f . snd)

seq_find :: (a -> Bool) -> [(t,a)] -> Maybe (t,a)
seq_find f = let f' (_,a) = f a in find f'

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

seq_coalesce :: Num t => (a -> a -> Bool) -> (a -> a -> a) -> [(t,a)] -> [(t,a)]
seq_coalesce dec_f jn_f =
    let dec_f' = dec_f `on` snd
        jn_f' (t1,a1) (t2,a2) = (t1 + t2,jn_f a1 a2)
    in coalesce_f dec_f' jn_f'

dseq_coalesce :: Num t => (a -> a -> Bool) -> (a -> a -> a) -> Dseq t a -> Dseq t a
dseq_coalesce = seq_coalesce

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

group_f :: (Eq t,Num t) => (t -> t -> Bool) -> [(t,a)] -> [(t,[a])]
group_f cmp =
    let recur t r sq =
            case sq of
              [] -> if null r then [] else [(t,reverse r)]
              (d,a):sq' -> if cmp t d
                           then recur t (a:r) sq'
                           else if null r
                                then recur d [a] sq'
                                else (t,reverse r) : recur d [a] sq'
    in recur 0 []

-- > let r = [(0,"a"),(1,"bc"),(2,"de"),(3,"f")]
-- > in tseq_group (zip [0,1,1,2,2,3] ['a'..]) == r
tseq_group :: (Eq t,Num t) => Tseq t a -> Tseq t [a]
tseq_group = group_f (==)

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

-- * Interop

-- > let r = zip [0,1,3,6,8,9] "abcde|"
-- > in dseq_to_tseq 0 '|' (zip [1,2,3,2,1] "abcde") == r
dseq_to_tseq :: Num t => t -> a -> Dseq t a -> Tseq t a
dseq_to_tseq t0 nil sq =
    let (d,a) = unzip sq
        t = T.dx_d t0 d
        a' = a ++ [nil]
    in zip t a'

-- | The last element of 'Tseq' is required to be an /eof/ marker that
-- has no duration and is not represented in the 'Dseq'.
--
-- > let r = zip [1,2,3,2,1] "abcde"
-- > in tseq_to_dseq (zip [0,1,3,6,8,9] "abcde|") == r
tseq_to_dseq :: Num t => Tseq t a -> Dseq t a
tseq_to_dseq sq =
    let (t,a) = unzip sq
        d = T.d_dx t
    in zip d a

tseq_to_iseq :: Num t => Tseq t a -> Dseq t a
tseq_to_iseq =
    let recur n p =
            case p of
              [] -> []
              (t,e):p' -> (t - n,e) : recur t p'
    in recur 0

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

-- * Type specialised map

dseq_tmap :: (t -> t') -> Dseq t a -> Dseq t' a
dseq_tmap = seq_tmap

pseq_tmap :: ((t,t,t) -> (t',t',t')) -> Pseq t a -> Pseq t' a
pseq_tmap = seq_tmap

tseq_tmap :: (t -> t') -> Dseq t a -> Dseq t' a
tseq_tmap = seq_tmap

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
