-- | Midi + Tuning
module Music.Theory.Tuning.Midi where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Data.Map as Map {- containers -}
import qualified Safe {- safe -}

import qualified Music.Theory.List as List {- hmt-base -}
import qualified Music.Theory.Map as Map {- hmt-base -}
import qualified Music.Theory.Tuple as Tuple {- hmt-base -}

import qualified Music.Theory.Pitch as Pitch {- hmt -}
import qualified Music.Theory.Tuning as Tuning {- hmt -}
import qualified Music.Theory.Tuning.Type as Tuning {- hmt -}

{- | (/n/ -> /dt/).
Function from midi note number /n/ to 'Midi_Detune' /dt/.
The incoming note number is the key pressed, which may be distant from the note sounded.
-}
type Midi_Tuning_f = Pitch.Midi -> Pitch.Midi_Detune

-- | Variant for tunings that are incomplete.
type Sparse_Midi_Tuning_f = Pitch.Midi -> Maybe Pitch.Midi_Detune

-- | Variant for sparse tunings that require state.
type Sparse_Midi_Tuning_St_f st = st -> Pitch.Midi -> (st, Maybe Pitch.Midi_Detune)

-- | Lift 'Midi_Tuning_f' to 'Sparse_Midi_Tuning_f'.
lift_tuning_f :: Midi_Tuning_f -> Sparse_Midi_Tuning_f
lift_tuning_f tn_f = Just . tn_f

-- | Lift 'Sparse_Midi_Tuning_f' to 'Sparse_Midi_Tuning_St_f'.
lift_sparse_tuning_f :: Sparse_Midi_Tuning_f -> Sparse_Midi_Tuning_St_f st
lift_sparse_tuning_f tn_f st k = (st, tn_f k)

{- | (t,c,k) where
t=tuning (must have 12 divisions of octave),
c=cents deviation (ie. constant detune offset),
k=midi offset (ie. value to be added to incoming midi note number).
-}
type D12_Midi_Tuning = (Tuning.Tuning, Tuning.Cents, Pitch.Midi)

{- | 'Midi_Tuning_f' for 'D12_Midi_Tuning'.

>>> let f = d12_midi_tuning_f (tn_equal_temperament 12,0,0)
>>> map f [0..127] == zip [0..127] (repeat 0)
True
-}
d12_midi_tuning_f :: D12_Midi_Tuning -> Midi_Tuning_f
d12_midi_tuning_f (t, c_diff, k) n =
  let (_, pc) = Pitch.midi_to_octpc (n + k)
      dt = zipWith (-) (Tuning.tn_cents t) [0, 100 .. 1200]
  in if Tuning.tn_divisions t /= 12
      then error "d12_midi_tuning_f: not d12"
      else case dt `Safe.atMay` pc of
        Nothing -> error "d12_midi_tuning_f: pc?"
        Just c -> (n, c + c_diff)

{- | (t,f0,k,g) where
t=tuning, f0=fundamental-frequency, k=midi-note-number (for f0), g=gamut
-}
type Cps_Midi_Tuning = (Tuning.Tuning, Double, Pitch.Midi, Int)

{- | 'Midi_Tuning_f' for 'Cps_Midi_Tuning'.
The function is sparse, it is only valid for /g/ values from /k/.

>>> let f = cps_midi_tuning_f (tn_equal_temperament 72,midi_to_cps 59,59,72 * 4)
>>> map (fst . fromJust . f) [59 .. 59 + 72]
[59,59,59,59,59,59,60,60,60,60,60,60,61,61,61,61,61,61,62,62,62,62,62,62,63,63,63,63,63,63,64,64,64,64,64,64,65,65,65,65,65,65,66,66,66,66,66,66,67,67,67,67,67,67,68,68,68,68,68,68,69,69,69,69,69,69,70,70,70,70,70,70,71]
-}
cps_midi_tuning_f :: Cps_Midi_Tuning -> Sparse_Midi_Tuning_f
cps_midi_tuning_f (t, f0, k, g) n =
  let r = Tuning.tn_approximate_ratios_cyclic t
      m = take g (map (Pitch.cps_to_midi_detune . (* f0)) r)
  in m `Safe.atMay` Pitch.midi_to_int (n - k)

-- * Midi tuning tables.

-- | midi-note-number -> fractional-midi-note-number table, possibly sparse.
type Mnn_Fmnn_Table = [(Int, Double)]

-- | Load 'Mnn_Fmnn_Table' from two-column Csv file.
mnn_fmnn_table_load_csv :: FilePath -> IO Mnn_Fmnn_Table
mnn_fmnn_table_load_csv fn = do
  s <- readFile fn
  let f x = case break (== ',') x of
        (lhs, _ : rhs) -> (read lhs, read rhs)
        _ -> error "mnn_fmidi_table_load_csv?"
  return (map f (lines s))

-- | Midi-note-number -> Cps table, possibly sparse.
type Mnn_Cps_Table = [(Pitch.Midi, Double)]

{- | Generates 'Mnn_Cps_Table' given 'Sparse_Midi_Tuning_f' with keys for all valid @Mnn@.

> import Sound.Sc3.Plot
> let f = cps_midi_tuning_f (equal_temperament 12,midi_to_cps 0,0,127)
> plot_p2_ln [map (fmap round) (gen_cps_tuning_tbl f)]

-}
gen_cps_tuning_tbl :: Sparse_Midi_Tuning_f -> Mnn_Cps_Table
gen_cps_tuning_tbl tn_f =
  let f n = case tn_f n of
        Just r -> Just (n, Pitch.midi_detune_to_cps r)
        Nothing -> Nothing
  in mapMaybe f [0 .. 127]

{- | Generates 'Mnn_Fmnn_Table' given 'Sparse_Midi_Tuning_f' with keys for all valid @Mnn@.

> import Music.Theory.Tuning.Scala
> t <- scl_load_tuning "bohlen-p"
> let f = cps_midi_tuning_f (t,midi_to_cps 0,0,128)
> map (\n -> let m = fromJust (f n) in (n, Pitch.midi_detune_to_fmidi m)) [0 .. 127]
-}
gen_fmnn_tuning_tbl :: Sparse_Midi_Tuning_f -> Mnn_Fmnn_Table
gen_fmnn_tuning_tbl tn_f =
  let f n = case tn_f n of
        Just r -> Just (n, Pitch.midi_detune_to_fmidi r)
        Nothing -> Nothing
  in mapMaybe f [0 .. 127]

-- * Derived (secondary) tuning table (DTT) lookup.

{- | Given an 'Mnn_Cps_Table' /tbl/, a list of @Cps@ /c/, and a @Mnn@ /m/
find the @Cps@ in /c/ that is nearest to the @Cps@ in /t/ for /m/.
In equal distance cases bias left.
-}
dtt_lookup :: (Eq k, Num v, Ord v) => [(k, v)] -> [v] -> k -> (Maybe v, Maybe v)
dtt_lookup tbl cps n =
  let f = lookup n tbl
  in (f, fmap (List.find_nearest_err True cps) f)

-- | Require table be non-sparse.
dtt_lookup_err :: (Eq k, Num v, Ord v) => [(k, v)] -> [v] -> k -> (k, v, v)
dtt_lookup_err tbl cps n =
  case dtt_lookup tbl cps n of
    (Just f, Just g) -> (n, f, g)
    _ -> error "dtt_lookup"

-- | Given two tuning tables generate the @dtt@ table.
gen_dtt_lookup_tbl :: Mnn_Cps_Table -> Mnn_Cps_Table -> Mnn_Cps_Table
gen_dtt_lookup_tbl t0 t1 =
  let ix = [0 .. 127]
      cps = sort (map (Tuple.p3_third . dtt_lookup_err t0 (map snd t1)) ix)
  in zip ix cps

gen_dtt_lookup_f :: Mnn_Cps_Table -> Mnn_Cps_Table -> Midi_Tuning_f
gen_dtt_lookup_f t0 t1 =
  let m = Map.fromList (gen_dtt_lookup_tbl t0 t1)
  in Pitch.cps_to_midi_detune . Map.map_ix_err m
