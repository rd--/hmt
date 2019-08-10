-- | Midi + Tuning
module Music.Theory.Tuning.Midi where

import Data.List {- base -}
import qualified Data.Map as M {- containers -}
import Data.Maybe {- base -}
import Data.Word {- base -}
import Safe {- safe -}

import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Map as T {- hmt -}
import qualified Music.Theory.Pitch as T {- hmt -}
import qualified Music.Theory.Tuple as T {- hmt -}

import Music.Theory.Tuning {- hmt -}

-- | (/n/ -> /dt/).  Function from midi note number /n/ to
-- 'Midi_Detune' /dt/.  The incoming note number is the key pressed,
-- which may be distant from the note sounded.
type Midi_Tuning_F = Int -> T.Midi_Detune

-- | Variant for tunings that are incomplete.
type Sparse_Midi_Tuning_F = Int -> Maybe T.Midi_Detune

-- | Variant for sparse tunings that require state.
type Sparse_Midi_Tuning_ST_F st = st -> Int -> (st,Maybe T.Midi_Detune)

-- | Lift 'Midi_Tuning_F' to 'Sparse_Midi_Tuning_F'.
lift_tuning_f :: Midi_Tuning_F -> Sparse_Midi_Tuning_F
lift_tuning_f tn_f = Just . tn_f

-- | Lift 'Sparse_Midi_Tuning_F' to 'Sparse_Midi_Tuning_ST_F'.
lift_sparse_tuning_f :: Sparse_Midi_Tuning_F -> Sparse_Midi_Tuning_ST_F st
lift_sparse_tuning_f tn_f st k = (st,tn_f k)

-- | (t,c,k) where
--   t=tuning (must have 12 divisions of octave),
--   c=cents deviation (ie. constant detune offset),
--   k=midi offset (ie. value to be added to incoming midi note number).
type D12_Midi_Tuning = (Tuning,Cents,Int)

-- | 'Midi_Tuning_F' for 'D12_Midi_Tuning'.
--
-- > let f = d12_midi_tuning_f (equal_temperament 12,0,0)
-- > map f [0..127] == zip [0..127] (repeat 0)
d12_midi_tuning_f :: D12_Midi_Tuning -> Midi_Tuning_F
d12_midi_tuning_f (t,c_diff,k) n =
    let (_,pc) = T.midi_to_octpc (n + k)
        dt = zipWith (-) (tn_cents t) [0,100 .. 1200]
    in if tn_divisions t /= 12
       then error "d12_midi_tuning_f: not d12"
       else case dt `atMay` pc of
              Nothing -> error "d12_midi_tuning_f: pc?"
              Just c -> (n,c + c_diff)

-- | (t,f0,k,g) where
--   t=tuning, f0=fundamental-frequency, k=midi-note-number (for f0), g=gamut
type CPS_Midi_Tuning = (Tuning,Double,Int,Int)

-- | 'Midi_Tuning_F' for 'CPS_Midi_Tuning'.  The function is sparse, it is only
-- valid for /g/ values from /k/.
--
-- > import qualified Music.Theory.Pitch as T
-- > let f = cps_midi_tuning_f (equal_temperament 72,T.midi_to_cps 59,59,72 * 4)
-- > map f [59 .. 59 + 72]
cps_midi_tuning_f :: CPS_Midi_Tuning -> Sparse_Midi_Tuning_F
cps_midi_tuning_f (t,f0,k,g) n =
    let r = tn_approximate_ratios_cyclic t
        m = take g (map (T.cps_to_midi_detune . (* f0)) r)
    in m `atMay` (n - k)

-- * Midi tuning tables.

-- | midi-note-number -> fractional-midi-note-number table, possibly sparse.
type MNN_FMNN_Table = [(Word8,Double)]

-- | Load 'MNN_FMNN_Table' from two-column CSV file.
mnn_fmnn_table_load_csv :: FilePath -> IO MNN_FMNN_Table
mnn_fmnn_table_load_csv fn = do
  s <- readFile fn
  let f x = case break (== ',') x of
              (lhs,_:rhs) -> (read lhs,read rhs)
              _ -> error "mnn_fmidi_table_load_csv?"
  return (map f (lines s))

-- | Midi-note-number -> CPS table, possibly sparse.
type MNN_CPS_Table = [(Int,Double)]

-- | Generates 'MNN_CPS_Table' given 'Midi_Tuning_F' with keys for all valid @MNN@.
--
-- > import Sound.SC3.Plot
-- > let f = cps_midi_tuning_f (equal_temperament 12,T.midi_to_cps 0,0,127)
-- > plot_p2_ln [map (fmap round) (gen_cps_tuning_tbl f)]
gen_cps_tuning_tbl :: Sparse_Midi_Tuning_F -> MNN_CPS_Table
gen_cps_tuning_tbl tn_f =
    let f n = case tn_f n of
                Just r -> Just (n,T.midi_detune_to_cps r)
                Nothing -> Nothing
    in mapMaybe f [0 .. 127]

-- * Derived (secondary) tuning table (DTT) lookup.

-- | Given an 'MNN_CPS_Table' /tbl/, a list of @CPS@ /c/, and a @MNN@ /m/
-- find the @CPS@ in /c/ that is nearest to the @CPS@ in /t/ for /m/.
-- In equal distance cases bias left.
dtt_lookup :: (Eq k, Num v, Ord v) => [(k,v)] -> [v] -> k -> (Maybe v,Maybe v)
dtt_lookup tbl cps n =
    let f = lookup n tbl
    in (f,fmap (T.find_nearest_err True cps) f)

-- | Require table be non-sparse.
dtt_lookup_err :: (Eq k, Num v, Ord v) => [(k,v)] -> [v] -> k -> (k,v,v)
dtt_lookup_err tbl cps n =
    case dtt_lookup tbl cps n of
      (Just f,Just g) -> (n,f,g)
      _ -> error "dtt_lookup"

-- | Given two tuning tables generate the @dtt@ table.
gen_dtt_lookup_tbl :: MNN_CPS_Table -> MNN_CPS_Table -> MNN_CPS_Table
gen_dtt_lookup_tbl t0 t1 =
    let ix = [0..127]
        cps = sort (map (T.p3_third . dtt_lookup_err t0 (map snd t1)) ix)
    in zip ix cps

gen_dtt_lookup_f :: MNN_CPS_Table -> MNN_CPS_Table -> Midi_Tuning_F
gen_dtt_lookup_f t0 t1 =
    let m = M.fromList (gen_dtt_lookup_tbl t0 t1)
    in T.cps_to_midi_detune . T.map_ix_err m
