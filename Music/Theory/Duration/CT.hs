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

ct_ext :: Int -> a -> [(Measure,a)] -> [(Measure,a)]
ct_ext n def sq = T.tseq_latch def sq [1 .. n]

ct_ext1 :: Int -> [(Measure,a)] -> [(Measure,a)]
ct_ext1 n sq =
    case sq of
      (1,e) : sq' -> ct_ext n e sq'
      _ -> error "ct_ext1"

ct_mdv_seq :: Int -> T.Tseq Measure T.Rational_Time_Signature -> [[T.RQ]]
ct_mdv_seq n ts = map (T.rts_divisions . snd) (ct_ext1 n ts)

ct_dv_seq :: Int -> T.Tseq Measure T.Rational_Time_Signature -> [(Int,[T.RQ])]
ct_dv_seq n ts = zip [1..] (ct_mdv_seq n ts)

ct_rq :: Int -> T.Tseq Measure T.Rational_Time_Signature -> [[T.RQ]]
ct_rq n ts = mdv_to_mrq (ct_mdv_seq n ts)

ct_mp_lookup :: [[T.RQ]] -> (Measure,Pulse) -> T.RQ
ct_mp_lookup = mp_lookup_err . mdv_to_mrq

ct_m_to_rq :: [[T.RQ]] -> [(Measure,t)] -> [(T.RQ,t)]
ct_m_to_rq sq = map (\(m,c) -> (ct_mp_lookup sq (m,1),c))

ct_mark_seq :: Int -> T.Tseq Measure Char -> T.Tseq Measure (Maybe Char)
ct_mark_seq n mk = T.seq_changed (ct_ext n '.' mk)

ct_pre_mark :: [(Int,a)] -> [(Measure,Maybe ())]
ct_pre_mark = map (\(m,_) -> (m - 1,Just ()))

ct_pre_mark_seq :: Measure -> T.Tseq Measure Char -> T.Tseq Measure (Maybe ())
ct_pre_mark_seq n mk =
    let pre = ct_pre_mark mk
    in T.tseq_merge_resolve const pre (zip [1 .. n] (repeat Nothing))

ct_tempo_lseq_rq :: [[T.RQ]] -> T.Lseq (Measure,Pulse) T.RQ -> T.Lseq T.RQ T.RQ
ct_tempo_lseq_rq sq = T.lseq_tmap (ct_mp_lookup sq)

ct_tempo_at :: T.Lseq T.RQ T.RQ -> T.RQ -> Rational
ct_tempo_at = T.lseq_lookup_err compare

data CT_Node = CT_Mark | CT_Start | CT_Normal | CT_Pre | CT_End
               deriving (Eq,Enum,Show)

ct_ix :: CT_Node -> (Int,Double)
ct_ix nd =
    case nd of
      CT_Mark -> (0,0.5)
      CT_Start -> (1,1)
      CT_Normal -> (2,1)
      CT_Pre -> (3,1)
      CT_End -> (0,1)

ct_leadin :: (Double,Int) -> T.Dseq Double [CT_Node]
ct_leadin (tm,n) = replicate n (60 / tm,[CT_Normal])

ct_measure:: T.Lseq T.RQ T.RQ -> ([T.RQ],Maybe Char,Maybe (),[T.RQ]) -> [(Rational,[CT_Node])]
ct_measure sq (mrq,mk,pr,dv) =
    let f (p,rq,du) = let nm = if p == 1
                               then case mk of
                                      Nothing -> [CT_Start]
                                      Just _ -> [CT_Mark,CT_Start]
                               else [if pr == Just () then CT_Pre else CT_Normal]
                      in (du * (60 / ct_tempo_at sq rq),nm)
    in map f (zip3 [1..] mrq dv)

data CT = CT {ct_len :: Int
             ,ct_ts :: [(Measure,T.Rational_Time_Signature)]
             ,ct_mark :: [(Measure,Char)]
             ,ct_tempo :: T.Lseq (Measure,Pulse) T.RQ
             ,ct_count :: Int
             ,ct_snd :: [FilePath]}

ct_tempo0 :: CT -> Maybe T.RQ
ct_tempo0 ct =
    case ct_tempo ct of
      (((1,1),_),n):_ -> Just n
      _ -> Nothing

ct_tempo0_err :: CT -> T.RQ
ct_tempo0_err = fromMaybe (error "ct_tempo0") . ct_tempo0

ct_measures :: CT -> [T.Dseq Rational [CT_Node]]
ct_measures (CT n ts mk tm _ _) =
    let f sq = let (m,v) = unzip sq
               in if m == [1 .. n] then v else error "ct_dseq"
        msr = zip4
              (f (zip [1..] (ct_rq n ts)))
              (f (ct_mark_seq n mk))
              (f (ct_pre_mark_seq n mk))
              (f (ct_dv_seq n ts))
    in map (ct_measure (ct_tempo_lseq_rq (ct_mdv_seq n ts) tm)) msr

ct_dseq' :: CT -> T.Dseq Rational [CT_Node]
ct_dseq' = concat . ct_measures

ct_dseq :: CT -> T.Dseq Double [CT_Node]
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
