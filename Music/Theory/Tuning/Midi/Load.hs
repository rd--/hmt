-- | Functions to load a tuning definition and transform it into a sparse tuning function.
module Music.Theory.Tuning.Midi.Load where

import Data.Maybe {- base -}

import qualified System.FilePath as FilePath {- filepath -}
import qualified System.Random {- random -}

import qualified Music.Theory.Array.Csv as Csv {- hmt-base -}

import qualified Music.Theory.Pitch as Pitch {- hmt -}
import qualified Music.Theory.Tuning as Tuning {- hmt -}
import qualified Music.Theory.Tuning.Midi as Midi {- hmt -}
import qualified Music.Theory.Tuning.Scala as Scala {- hmt -}
import qualified Music.Theory.Tuning.Type as Tuning {- hmt -}

{- | Load possibly sparse and possibly one-to-many
(midi-note-number,cps-frequency) table from Csv file.

>>> load_cps_tbl "/home/rohan/sw/hmt/data/csv/tuning/Rosenboon1979.csv"
[(32,51.91308719749315),(37,69.21744959665753),(38,75.50994501453549),(40,83.06093951598905),(42,92.28993279554336),(44,103.8261743949863),(46,118.65848502284148),(49,138.43489919331506),(51,155.73926159247944),(52,166.1218790319781),(54,184.57986559108673),(56,207.6523487899726),(58,233.60889238871917),(60,259.5654359874657),(61,276.8697983866301),(62,285.5219795862123),(63,311.4785231849589),(64,332.2437580639562),(66,363.39161038245203)]
-}
load_cps_tbl :: FilePath -> IO [(Pitch.Midi, Double)]
load_cps_tbl nm = do
  tbl <- Csv.csv_table_read_def id nm
  let f e = case e of
        [p, q] -> (read p, read q)
        _ -> error "load_cps_tbl"
  return (map f tbl)

{- | Load scala scl file as 'Tuning.Tuning'.

>>> t <- load_tuning_scl "meanquar"
>>> Tuning.tn_cents_i t
[0,76,193,310,386,503,579,697,773,890,1007,1083]
-}
load_tuning_scl :: String -> IO Tuning.Tuning
load_tuning_scl = Scala.scl_load_tuning

{- | There are two forms.

For cps = (tuning-name,frequency-zero,midi-note-number-of-f0)

For d12 = (tuning-name,cents-deviation,midi-note-offset)
-}
type Load_Tuning_Opt = (String, Double, Pitch.Midi)

{- | Load scala file and apply `cps_midi_tuning_f`.

>>> f <- load_tuning_cps ("mersen_s1", Tuning.midi_to_cps 0, 0)
>>> map (round . Pitch.midi_detune_to_cps . Data.Maybe.fromJust . f) [60 .. 72]
[262,279,291,314,327,349,372,392,419,436,465,491,523]
-}
load_tuning_cps :: Load_Tuning_Opt -> IO Midi.Sparse_Midi_Tuning_f
load_tuning_cps (nm, f0, k) =
  let f tn = Midi.cps_midi_tuning_f (tn, f0, k, 128 - Pitch.midi_to_int k)
  in fmap f (load_tuning_scl nm)

-- | Load scala file and apply 'Tuning.d12_midi_tuning_f'.
load_tuning_d12 :: Load_Tuning_Opt -> IO Midi.Sparse_Midi_Tuning_f
load_tuning_d12 (nm, dt, k) =
  let f tn = Midi.lift_tuning_f (Midi.d12_midi_tuning_f (tn, dt, k))
  in fmap f (load_tuning_scl nm)

-- | Lookup first matching element in table.
load_tuning_tbl :: Load_Tuning_Opt -> IO Midi.Sparse_Midi_Tuning_f
load_tuning_tbl (nm, dt, k) =
  let from_cps = Pitch.cps_to_midi_detune . flip Tuning.cps_shift_cents dt
      f tbl mnn = fmap from_cps (lookup (mnn + k) tbl)
  in fmap f (load_cps_tbl nm)

type Choose_f st t = [t] -> st -> (t, st)

-- | Randomly choose from elements in table, equal weighting.
default_choose_f :: System.Random.RandomGen g => Choose_f g t
default_choose_f l g =
  let (i, g') = System.Random.randomR (0, length l - 1) g
  in (l !! i, g')

-- | Load tuning table with stateful selection function for one-to-many entries.
load_tuning_tbl_st :: Choose_f st (Pitch.Midi, Double) -> Load_Tuning_Opt -> IO (Midi.Sparse_Midi_Tuning_St_f st)
load_tuning_tbl_st choose_f (nm, dt, k) =
  let from_cps = Pitch.cps_to_midi_detune . flip Tuning.cps_shift_cents dt
      f tbl g mnn = case filter ((== (mnn + k)) . fst) tbl of
        [] -> (g, Nothing)
        l ->
          let ((_, e), g') = choose_f l g
          in (g', Just (from_cps e))
  in fmap f (load_cps_tbl nm)

load_tuning_ty :: String -> Load_Tuning_Opt -> IO Midi.Sparse_Midi_Tuning_f
load_tuning_ty ty opt =
  case ty of
    "cps" -> load_tuning_cps opt
    "d12" -> load_tuning_d12 opt
    "tbl" -> load_tuning_tbl opt
    _ -> error "cps|d12|tbl"

load_tuning_st_ty :: String -> Load_Tuning_Opt -> IO (Midi.Sparse_Midi_Tuning_St_f System.Random.StdGen)
load_tuning_st_ty ty opt =
  case ty of
    "cps" -> fmap Midi.lift_sparse_tuning_f (load_tuning_cps opt)
    "d12" -> fmap Midi.lift_sparse_tuning_f (load_tuning_d12 opt)
    "tbl" -> load_tuning_tbl_st default_choose_f opt
    _ -> error "cps|d12|tbl"

-- * Csv or Scl

-- | Load 'Mnn_Fmnn_Table' from two-column Csv file.
mnn_fmnn_table_load_csv :: FilePath -> IO Midi.Mnn_Fmnn_Table
mnn_fmnn_table_load_csv fn = do
  s <- readFile fn
  let f x = case break (== ',') x of
        (lhs, _ : rhs) -> (read lhs, read rhs)
        _ -> error "mnn_fmidi_table_load_csv?"
  return (map f (lines s))

-- | Load 'Mnn_Fmnn_Table' from Scala tuning file.
mnn_fmnn_table_load_scl :: String -> IO Midi.Mnn_Fmnn_Table
mnn_fmnn_table_load_scl nm = do
  f <- load_tuning_cps (nm, Tuning.midi_to_cps (0::Int), 0)
  let mnn = [0 .. 127]
      fmnn = map (Pitch.midi_detune_to_fmidi . Data.Maybe.fromJust . f) mnn
  return (zip mnn fmnn)

{- | Load 'Mnn_Fmnn_Table' from either a Csv file or a Scala file.
If there is no file extension the name is assumed to be a Scala file name.

>>> tbl <- mnn_fmnn_table_load_csv_or_scl "mersen_s1.scl"
>>> take 4 (drop 60 tbl)
[(60,60.0),(61,61.11731285269777),(62,61.824037121340595),(63,63.156412870005525)]
-}
mnn_fmnn_table_load_csv_or_scl :: String -> IO Midi.Mnn_Fmnn_Table
mnn_fmnn_table_load_csv_or_scl nm =
  case FilePath.takeExtension nm of
    ".csv" -> mnn_fmnn_table_load_csv nm
    ".scl" -> mnn_fmnn_table_load_scl (FilePath.dropExtension nm)
    "" -> mnn_fmnn_table_load_scl nm
    _ -> error "mnn_fmnn_table_load_csv_or_scl: invalid extension"
