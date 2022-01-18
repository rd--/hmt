-- | Functions to generate a click track from a metric structure.
module Music.Theory.Duration.CT where

import Data.Bifunctor {- base -}
import Data.Function {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.List as List {- hmt-base -}

import qualified Music.Theory.Duration.RQ as T {- hmt -}
import qualified Music.Theory.Time_Signature as T {- hmt -}
import qualified Music.Theory.Time.Seq as T {- hmt -}

-- | 1-indexed.
type Measure = Int

-- | 1-indexed.
type Pulse = Int

-- | Measures given as 'T.RQ' divisions, Mdv abbreviates measure divisions.
type Mdv = [[T.RQ]]

{- | Absolute 'T.RQ' locations grouped in measures.
     mrq abbreviates measure rational quarter-notes.
     Locations are zero-indexed.
-}
type Mrq = [[T.RQ]]

{- | Transform Mdv to Mrq.

> mdv_to_mrq [[1,2,1],[3,2,1]] == [[0,1,3],[4,7,9]]
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

-- * CT

{- | Latch measures (ie. make measures contiguous, hold previous value).
     Arguments are the number of measures and the default (intial) value.

> unzip (ct_ext 10 'a' [(3,'b'),(8,'c')]) == ([1..10],"aabbbbbccc")
-}
ct_ext :: Int -> t -> T.Tseq Measure t -> T.Tseq Measure t
ct_ext n def sq = T.tseq_latch def sq [1 .. n]

-- | Variant that requires a value at measure one (first measure).
ct_ext1 :: Int -> T.Tseq Measure t -> T.Tseq Measure t
ct_ext1 n sq =
    case sq of
      (1,e) : sq' -> ct_ext n e sq'
      _ -> error "ct_ext1"

-- | 'T.rts_divisions' of 'ct_ext1'.
ct_dv_seq :: Int -> T.Tseq Measure T.Rational_Time_Signature -> [(Measure,[[T.RQ]])]
ct_dv_seq n ts = map (fmap T.rts_divisions) (ct_ext1 n ts)

-- | 'ct_dv_seq' without measures numbers (which are 1..n)
ct_mdv_seq :: Int -> T.Tseq Measure T.Rational_Time_Signature -> [[T.RQ]]
ct_mdv_seq n = map (concat . snd) . ct_dv_seq n

-- | 'mdv_to_mrq' of 'ct_mdv_seq'.
ct_rq :: Int -> T.Tseq Measure T.Rational_Time_Signature -> [[T.RQ]]
ct_rq n ts = mdv_to_mrq (ct_mdv_seq n ts)

ct_mp_lookup :: [[T.RQ]] -> (Measure,Pulse) -> T.RQ
ct_mp_lookup = mp_lookup_err . mdv_to_mrq

ct_m_to_rq :: [[T.RQ]] -> [(Measure,t)] -> [(T.RQ,t)]
ct_m_to_rq sq = map (\(m,c) -> (ct_mp_lookup sq (m,1),c))

-- | Latch rehearsal mark sequence, only indicating marks.  Initial mark is @.@.
--
-- > ct_mark_seq 2 [] == [(1,Just '.'),(2,Nothing)]
--
-- > let r = [(1,Just '.'),(3,Just 'A'),(8,Just 'B')]
-- > in filter (isJust . snd) (ct_mark_seq 10 [(3,'A'),(8,'B')]) == r
ct_mark_seq :: Int -> T.Tseq Measure Char -> T.Tseq Measure (Maybe Char)
ct_mark_seq n mk = T.seq_changed (ct_ext n '.' mk)

-- | Indicate measures prior to marks.
--
-- > ct_pre_mark [] == []
-- > ct_pre_mark [(1,'A')] == []
-- > ct_pre_mark [(3,'A'),(8,'B')] == [(2,Just ()),(7,Just ())]
ct_pre_mark :: [(Measure,a)] -> [(Measure,Maybe ())]
ct_pre_mark = mapMaybe (\(m,_) -> if m <= 1 then Nothing else Just (m - 1,Just ()))

-- | Contiguous pre-mark sequence.
--
-- > ct_pre_mark_seq 1 [(1,'A')] == [(1,Nothing)]
-- > ct_pre_mark_seq 10 [(3,'A'),(8,'B')]
ct_pre_mark_seq :: Measure -> T.Tseq Measure Char -> T.Tseq Measure (Maybe ())
ct_pre_mark_seq n mk =
    let pre = ct_pre_mark mk
    in T.tseq_merge_resolve const pre (zip [1 .. n] (repeat Nothing))

ct_tempo_lseq_rq :: [[T.RQ]] -> T.Lseq (Measure,Pulse) T.RQ -> T.Lseq T.RQ T.RQ
ct_tempo_lseq_rq sq = T.lseq_tmap (ct_mp_lookup sq)

-- | Interpolating lookup of tempo sequence ('T.lseq_lookup_err').
ct_tempo_at :: T.Lseq T.RQ T.RQ -> T.RQ -> Rational
ct_tempo_at = T.lseq_lookup_err compare

-- | Types of nodes.
data CT_Node = CT_Mark T.RQ -- ^ The start of a measure with a rehearsal mark.
             | CT_Start T.RQ -- ^ The start of a regular measure.
             | CT_Normal T.RQ -- ^ A regular pulse.
             | CT_Edge T.RQ -- ^ The start of a pulse group within a measure.
             | CT_Pre T.RQ -- ^ A regular pulse in a measure prior to a rehearsal mark.
             | CT_End -- ^ The end of the track.
               deriving (Eq,Show)

-- | Lead-in of @(pulse,tempo,count)@.
ct_leadin :: (T.RQ,Double,Int) -> T.Dseq Double CT_Node
ct_leadin (du,tm,n) = replicate n (realToFrac du * (60 / tm),CT_Normal du)

-- | Prepend initial element to start of list.
--
-- > delay1 "abc" == "aabc"
delay1 :: [a] -> [a]
delay1 l =
    case l of
      [] -> error "delay1: []"
      e:_ -> e : l

{- | Generate CT measure.
     Calculates durations of events considering only the tempo at the start of the event.
     To be correct it should consider the tempo envelope through the event.
-}
ct_measure:: T.Lseq T.RQ T.RQ -> ([T.RQ],Maybe Char,Maybe (),[[T.RQ]]) -> [(Rational,CT_Node)]
ct_measure sq (mrq,mk,pr,dv) =
    let dv' = concatMap (zip [1::Int ..]) dv
        f (p,rq,(g,du)) =
            let nm = if p == 1
                     then case mk of
                            Nothing -> CT_Start du
                            Just _ -> CT_Mark du
                     else if pr == Just ()
                          then CT_Pre du
                          else if g == 1 then CT_Edge du else CT_Normal du
            in (du * (60 / ct_tempo_at sq rq),nm)
    in map f (zip3 [1::Int ..] mrq dv')

-- | Click track definition.
data CT = CT {ct_len :: Int
             ,ct_ts :: [(Measure,T.Rational_Time_Signature)]
             ,ct_mark :: [(Measure,Char)]
             ,ct_tempo :: T.Lseq (Measure,Pulse) T.RQ
             ,ct_count :: (T.RQ,Int)}
          deriving Show

-- | Initial tempo, if given.
ct_tempo0 :: CT -> Maybe T.RQ
ct_tempo0 ct =
    case ct_tempo ct of
      (((1,1),_),n):_ -> Just n
      _ -> Nothing

-- | Erroring variant.
ct_tempo0_err :: CT -> T.RQ
ct_tempo0_err = fromMaybe (error "ct_tempo0") . ct_tempo0

-- > import Music.Theory.Duration.CT
-- > import Music.Theory.Time.Seq
-- > let ct = CT 2 [(1,[(3,8),(2,4)])] [(1,'a')] [(((1,1),T.None),60)] undefined
-- > ct_measures ct
ct_measures :: CT -> [T.Dseq Rational CT_Node]
ct_measures (CT n ts mk tm _) =
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

ct_dseq' :: CT -> T.Dseq Rational CT_Node
ct_dseq' = concat . ct_measures

ct_dseq :: CT -> T.Dseq Double CT_Node
ct_dseq = T.dseq_tmap fromRational . ct_dseq'

-- * Indirect

ct_rq_measure :: [[T.RQ]] -> T.RQ -> Maybe Measure
ct_rq_measure sq rq = fmap fst (find ((rq `elem`) . snd) (zip [1..] sq))

ct_rq_mp :: [[T.RQ]] -> T.RQ -> Maybe (Measure,Pulse)
ct_rq_mp sq rq =
    let f (m,l) = (m,fromMaybe (error "ct_rq_mp: ix") (elemIndex rq l) + 1)
    in fmap f (find ((rq `elem`) . snd) (zip [1..] sq))

ct_rq_mp_err :: [[T.RQ]] -> T.RQ -> (Measure, Pulse)
ct_rq_mp_err sq = fromMaybe (error "ct_rq_mp") . ct_rq_mp sq

ct_mp_to_rq :: [[T.RQ]] -> [((Measure,Pulse),t)] -> [(T.RQ,t)]
ct_mp_to_rq sq = map (first (ct_mp_lookup sq))
