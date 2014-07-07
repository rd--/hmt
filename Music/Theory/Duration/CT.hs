-- | Functions to generate a click track from a metric structure.
module Music.Theory.Duration.CT where

import Data.Function {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.Duration.RQ as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Time_Signature as T {- hmt -}
import qualified Music.Theory.Time.Seq as T {- hmt -}

-- | 1-indexed.
type Measure = Int

-- | 1-indexed.
type Pulse = Int

-- | Transform measures given as 'T.RQ' divisions to absolute 'T.RQ'
-- locations.  /mdv/ abbreviates measure divisions.
--
-- > mdv_to_mrq [[1,2,1],[3,2,1]] == [[0,1,3],[4,7,9]]
mdv_to_mrq :: [[T.RQ]] -> [[T.RQ]]
mdv_to_mrq = snd . mapAccumL T.dx_d' 0

-- | Lookup function for ('Measure','Pulse') indexed structure.
mp_lookup_err :: [[a]] -> (Measure,Pulse) -> a
mp_lookup_err sq (m,p) = (sq !! (m - 1)) !! (p - 1)

-- | Comparison for ('Measure','Pulse') indices.
mp_compare :: (Measure,Pulse) -> (Measure,Pulse) -> Ordering
mp_compare = T.two_stage_compare (compare `on` fst) (compare `on` snd)

-- * CT

-- | Latch measures (ie. make measures contiguous, hold previous value).
--
-- > unzip (ct_ext 10 'a' [(3,'b'),(8,'c')]) == ([1..10],"aabbbbbccc")
ct_ext :: Int -> a -> [(Measure,a)] -> [(Measure,a)]
ct_ext n def sq = T.tseq_latch def sq [1 .. n]

-- | Variant that requires a value at measure one (first measure).
ct_ext1 :: Int -> [(Measure,a)] -> [(Measure,a)]
ct_ext1 n sq =
    case sq of
      (1,e) : sq' -> ct_ext n e sq'
      _ -> error "ct_ext1"

-- | 'T.rts_divisions' of 'ct_ext1'.
ct_dv_seq :: Int -> T.Tseq Measure T.Rational_Time_Signature -> [(Measure,[T.RQ])]
ct_dv_seq n ts = map (fmap T.rts_divisions) (ct_ext1 n ts)

-- | 'ct_dv_seq' without measures numbers.
ct_mdv_seq :: Int -> T.Tseq Measure T.Rational_Time_Signature -> [[T.RQ]]
ct_mdv_seq n = map snd . ct_dv_seq n

-- | 'mdv_to_mrq' of 'ct_mdv_seq'.
ct_rq :: Int -> T.Tseq Measure T.Rational_Time_Signature -> [[T.RQ]]
ct_rq n ts = mdv_to_mrq (ct_mdv_seq n ts)

ct_mp_lookup :: [[T.RQ]] -> (Measure,Pulse) -> T.RQ
ct_mp_lookup = mp_lookup_err . mdv_to_mrq

ct_m_to_rq :: [[T.RQ]] -> [(Measure,t)] -> [(T.RQ,t)]
ct_m_to_rq sq = map (\(m,c) -> (ct_mp_lookup sq (m,1),c))

-- | Latch rehearsal mark sequence, only indicating marks.  Initial mark is @.@.
--
-- > let r = [(1,Just '.'),(3,Just 'A'),(8,Just 'B')]
-- > in filter (isJust . snd) (ct_mark_seq 10 [(3,'A'),(8,'B')]) == r
ct_mark_seq :: Int -> T.Tseq Measure Char -> T.Tseq Measure (Maybe Char)
ct_mark_seq n mk = T.seq_changed (ct_ext n '.' mk)

-- | Indicate measures prior to marks.
--
-- > ct_pre_mark [(3,'A'),(8,'B')] == [(2,Just ()),(7,Just ())]
ct_pre_mark :: [(Measure,a)] -> [(Measure,Maybe ())]
ct_pre_mark = map (\(m,_) -> (m - 1,Just ()))

-- | Contiguous pre-mark sequence.
--
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

ct_measure:: T.Lseq T.RQ T.RQ -> ([T.RQ],Maybe Char,Maybe (),[T.RQ]) -> [(Rational,CT_Node)]
ct_measure sq (mrq,mk,pr,dv) =
    let f (p,rq,du,du') =
            let nm = if p == 1
                     then case mk of
                            Nothing -> CT_Start du
                            Just _ -> CT_Mark du
                     else if pr == Just ()
                          then CT_Pre du
                          else if du == du' then CT_Normal du else CT_Edge du
            in (du * (60 / ct_tempo_at sq rq),nm)
    in map f (zip4 [1..] mrq dv (delay1 dv))

-- | Click track definition.
data CT = CT {ct_len :: Int
             ,ct_ts :: [(Measure,T.Rational_Time_Signature)]
             ,ct_mark :: [(Measure,Char)]
             ,ct_tempo :: T.Lseq (Measure,Pulse) T.RQ
             ,ct_count :: (T.RQ,Int)}

-- | Initial tempo, if given.
ct_tempo0 :: CT -> Maybe T.RQ
ct_tempo0 ct =
    case ct_tempo ct of
      (((1,1),_),n):_ -> Just n
      _ -> Nothing

-- | Erroring variant.
ct_tempo0_err :: CT -> T.RQ
ct_tempo0_err = fromMaybe (error "ct_tempo0") . ct_tempo0

ct_measures :: CT -> [T.Dseq Rational CT_Node]
ct_measures (CT n ts mk tm _) =
    let f sq = let (m,v) = unzip sq
               in if m == [1 .. n] then v else error "ct_dseq"
        msr = zip4
              (f (zip [1..] (ct_rq n ts)))
              (f (ct_mark_seq n mk))
              (f (ct_pre_mark_seq n mk))
              (f (ct_dv_seq n ts))
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
    let f (m,l) = (m,fromMaybe (error "ct_rq_mp: ix") (findIndex (== rq) l) + 1)
    in fmap f (find ((rq `elem`) . snd) (zip [1..] sq))

ct_rq_mp_err :: [[T.RQ]] -> T.RQ -> (Measure, Pulse)
ct_rq_mp_err sq = fromMaybe (error "ct_rq_mp") . ct_rq_mp sq

ct_mp_to_rq :: [[T.RQ]] -> [((Measure,Pulse),t)] -> [(T.RQ,t)]
ct_mp_to_rq sq = map (\(mp,c) -> (ct_mp_lookup sq mp,c))
