-- | Functions to generate a click track from a metric structure.
module Music.Theory.Duration.ClickTrack where

import Data.Bifunctor {- base -}
import Data.Function {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.List as List {- hmt-base -}

import qualified Music.Theory.Duration.Rq as Rq {- hmt -}
import qualified Music.Theory.Time_Signature as Time_Signature {- hmt -}
import qualified Music.Theory.Time.Seq as Seq {- hmt -}

-- | 1-indexed.
type Measure = Int

-- | 1-indexed.
type Pulse = Int

-- | Measures given as 'Rq.Rq' divisions, Mdv abbreviates measure divisions.
type Mdv = [[Rq.Rq]]

{- | Absolute 'Rq.Rq' locations grouped in measures.
     mrq abbreviates measure rational quarter-notes.
     Locations are zero-indexed.
-}
type Mrq = [[Rq.Rq]]

{- | Transform Mdv to Mrq.

>>> mdv_to_mrq [[1,2,1],[3,2,1]] == [[0,1,3],[4,7,9]]
True
-}
mdv_to_mrq :: Mdv -> Mrq
mdv_to_mrq = snd . mapAccumL List.dx_d' 0

{- | Lookup function for ('Measure','Pulse') indexed structure.
     mp abbreviates Measure Pulse.
-}
mp_lookup_err :: [[t]] -> (Measure,Pulse) -> t
mp_lookup_err sq (m,p) =
    if m < 1 || p < 1
    then error (show ("mp_lookup_err: one indexed?",m,p))
    else (sq !! (m - 1)) !! (p - 1)

-- | Comparison for ('Measure','Pulse') indices.
mp_compare :: (Measure,Pulse) -> (Measure,Pulse) -> Ordering
mp_compare = List.two_stage_compare (compare `on` fst) (compare `on` snd)

-- * Ct

{- | Latch measures (ie. make measures contiguous, hold previous value).
     Arguments are the number of measures and the default (intial) value.

>>> unzip (ct_ext 10 'a' [(3,'b'),(8,'c')]) == ([1..10],"aabbbbbccc")
True
-}
ct_ext :: Int -> t -> Seq.Tseq Measure t -> Seq.Tseq Measure t
ct_ext n def sq = Seq.tseq_latch def sq [1 .. n]

-- | Variant that requires a value at measure one (first measure).
ct_ext1 :: Int -> Seq.Tseq Measure t -> Seq.Tseq Measure t
ct_ext1 n sq =
    case sq of
      (1,e) : sq' -> ct_ext n e sq'
      _ -> error "ct_ext1"

-- | 'Rq.rts_divisions' of 'ct_ext1'.
ct_dv_seq :: Int -> Seq.Tseq Measure Time_Signature.Rational_Time_Signature -> [(Measure,[[Rq.Rq]])]
ct_dv_seq n ts = map (fmap Time_Signature.rts_divisions) (ct_ext1 n ts)

-- | 'ct_dv_seq' without measures numbers (which are 1..n)
ct_mdv_seq :: Int -> Seq.Tseq Measure Time_Signature.Rational_Time_Signature -> [[Rq.Rq]]
ct_mdv_seq n = map (concat . snd) . ct_dv_seq n

-- | 'mdv_to_mrq' of 'ct_mdv_seq'.
ct_rq :: Int -> Seq.Tseq Measure Time_Signature.Rational_Time_Signature -> [[Rq.Rq]]
ct_rq n ts = mdv_to_mrq (ct_mdv_seq n ts)

ct_mp_lookup :: [[Rq.Rq]] -> (Measure,Pulse) -> Rq.Rq
ct_mp_lookup = mp_lookup_err . mdv_to_mrq

ct_m_to_rq :: [[Rq.Rq]] -> [(Measure,t)] -> [(Rq.Rq,t)]
ct_m_to_rq sq = map (\(m,c) -> (ct_mp_lookup sq (m,1),c))

{- | Latch rehearsal mark sequence, only indicating marks.  Initial mark is @.@.

>>> ct_mark_seq 2 []
[(1,Just '.'),(2,Nothing)]

>>> filter (isJust . snd) (ct_mark_seq 10 [(3,'A'),(8,'B')])
[(1,Just '.'),(3,Just 'A'),(8,Just 'B')]
-}
ct_mark_seq :: Int -> Seq.Tseq Measure Char -> Seq.Tseq Measure (Maybe Char)
ct_mark_seq n mk = Seq.seq_changed (ct_ext n '.' mk)

{- | Indicate measures prior to marks.

>>> ct_pre_mark []
[]

>>> ct_pre_mark [(1,'A')]
[]

>>> ct_pre_mark [(3,'A'),(8,'B')]
[(2,Just ()),(7,Just ())]
-}
ct_pre_mark :: [(Measure,a)] -> [(Measure,Maybe ())]
ct_pre_mark = mapMaybe (\(m,_) -> if m <= 1 then Nothing else Just (m - 1,Just ()))

{- | Contiguous pre-mark sequence.

>>> ct_pre_mark_seq 1 [(1,'A')]
[(1,Nothing)]

>>> filter (\(_,x) -> x /= Nothing) (ct_pre_mark_seq 10 [(3,'A'),(8,'B')])
[(2,Just ()),(7,Just ())]
-}
ct_pre_mark_seq :: Measure -> Seq.Tseq Measure Char -> Seq.Tseq Measure (Maybe ())
ct_pre_mark_seq n mk =
    let pre = ct_pre_mark mk
    in Seq.tseq_merge_resolve const pre (zip [1 .. n] (repeat Nothing))

ct_tempo_lseq_rq :: [[Rq.Rq]] -> Seq.Lseq (Measure,Pulse) Rq.Rq -> Seq.Lseq Rq.Rq Rq.Rq
ct_tempo_lseq_rq sq = Seq.lseq_tmap (ct_mp_lookup sq)

-- | Interpolating lookup of tempo sequence ('Seq.lseq_lookup_err').
ct_tempo_at :: Seq.Lseq Rq.Rq Rq.Rq -> Rq.Rq -> Rational
ct_tempo_at = Seq.lseq_lookup_err compare

-- | Types of nodes.
data Ct_Node = Ct_Mark Rq.Rq -- ^ The start of a measure with a rehearsal mark.
             | Ct_Start Rq.Rq -- ^ The start of a regular measure.
             | Ct_Normal Rq.Rq -- ^ A regular pulse.
             | Ct_Edge Rq.Rq -- ^ The start of a pulse group within a measure.
             | Ct_Pre Rq.Rq -- ^ A regular pulse in a measure prior to a rehearsal mark.
             | Ct_End -- ^ The end of the track.
               deriving (Eq,Show)

-- | Lead-in of @(pulse,tempo,count)@.
ct_leadin :: (Rq.Rq,Double,Int) -> Seq.Dseq Double Ct_Node
ct_leadin (du,tm,n) = replicate n (realToFrac du * (60 / tm),Ct_Normal du)

{- | Prepend initial element to start of list.

>>> delay1 "abc"
"aabc"
-}
delay1 :: [a] -> [a]
delay1 l =
    case l of
      [] -> error "delay1: []"
      e:_ -> e : l

{- | Generate Ct measure.
     Calculates durations of events considering only the tempo at the start of the event.
     To be correct it should consider the tempo envelope through the event.
-}
ct_measure:: Seq.Lseq Rq.Rq Rq.Rq -> ([Rq.Rq],Maybe Char,Maybe (),[[Rq.Rq]]) -> [(Rational,Ct_Node)]
ct_measure sq (mrq,mk,pr,dv) =
    let dv' = concatMap (zip [1::Int ..]) dv
        f (p,rq,(g,du)) =
            let nm = if p == 1
                     then case mk of
                            Nothing -> Ct_Start du
                            Just _ -> Ct_Mark du
                     else if pr == Just ()
                          then Ct_Pre du
                          else if g == 1 then Ct_Edge du else Ct_Normal du
            in (du * (60 / ct_tempo_at sq rq),nm)
    in map f (zip3 [1::Int ..] mrq dv')

-- | Click track definition.
data Ct =
  Ct
  {ct_len :: Int
  ,ct_ts :: [(Measure,Time_Signature.Rational_Time_Signature)]
  ,ct_mark :: [(Measure,Char)]
  ,ct_tempo :: Seq.Lseq (Measure,Pulse) Rq.Rq
  ,ct_count :: (Rq.Rq,Int)}
  deriving Show

-- | Initial tempo, if given.
ct_tempo0 :: Ct -> Maybe Rq.Rq
ct_tempo0 ct =
    case ct_tempo ct of
      (((1,1),_),n):_ -> Just n
      _ -> Nothing

-- | Erroring variant.
ct_tempo0_err :: Ct -> Rq.Rq
ct_tempo0_err = fromMaybe (error "ct_tempo0") . ct_tempo0

{- | Measures

> import Music.Theory.Duration.Ct
> import Music.Theory.Time.Seq
> let ct = Ct 2 [(1,[(3,8),(2,4)])] [(1,'a')] [(((1,1),Rq.None),60)] undefined
> ct_measures ct
-}
ct_measures :: Ct -> [Seq.Dseq Rational Ct_Node]
ct_measures (Ct n ts mk tm _) =
    let f msg sq = let (m,v) = unzip sq
                   in if m == [1 .. n]
                      then v
                      else error (show ("ct_measures",msg,sq,m,v,n))
        msr = zip4
              (f "ts" (zip [1..] (ct_rq n ts)))
              (f "mk" (ct_mark_seq n mk))
              (f "pre-mk" (ct_pre_mark_seq n mk))
              (f "dv" (ct_dv_seq n ts))
    in map (ct_measure (ct_tempo_lseq_rq (ct_mdv_seq n ts) tm)) msr

ct_dseq' :: Ct -> Seq.Dseq Rational Ct_Node
ct_dseq' = concat . ct_measures

ct_dseq :: Ct -> Seq.Dseq Double Ct_Node
ct_dseq = Seq.dseq_tmap fromRational . ct_dseq'

-- * Indirect

ct_rq_measure :: [[Rq.Rq]] -> Rq.Rq -> Maybe Measure
ct_rq_measure sq rq = fmap fst (find ((rq `elem`) . snd) (zip [1..] sq))

ct_rq_mp :: [[Rq.Rq]] -> Rq.Rq -> Maybe (Measure,Pulse)
ct_rq_mp sq rq =
    let f (m,l) = (m,fromMaybe (error "ct_rq_mp: ix") (elemIndex rq l) + 1)
    in fmap f (find ((rq `elem`) . snd) (zip [1..] sq))

ct_rq_mp_err :: [[Rq.Rq]] -> Rq.Rq -> (Measure, Pulse)
ct_rq_mp_err sq = fromMaybe (error "ct_rq_mp") . ct_rq_mp sq

ct_mp_to_rq :: [[Rq.Rq]] -> [((Measure,Pulse),t)] -> [(Rq.Rq,t)]
ct_mp_to_rq sq = map (first (ct_mp_lookup sq))
