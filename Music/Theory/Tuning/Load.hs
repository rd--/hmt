module Music.Theory.Tuning.Load where

import qualified Music.Theory.Array.CSV as T
import qualified Music.Theory.Pitch as T
import qualified Music.Theory.Tuning as T
import qualified Music.Theory.Tuning.Scala as T

-- | Load possibly sparse (midi-note-number,cps-frequency) table from CSV file.
--
-- > load_cps_tbl "/home/rohan/dr.csv"
load_cps_tbl :: FilePath -> IO [(Int,Double)]
load_cps_tbl nm = do
  tbl <- T.csv_table_read' id nm
  let f e = case e of
              [p,q] -> (read p,read q)
              _ -> error "load_cps_tbl"
  return (map f tbl)

load_tuning_scl :: String -> IO T.Tuning
load_tuning_scl = fmap (T.scale_to_tuning 0.01) . T.scl_load

load_tuning_cps :: (String,Double,Int) -> IO T.Sparse_Midi_Tuning_F
load_tuning_cps (nm,f0,k) =
    let f tn = T.cps_midi_tuning_f (tn,f0,k,128-k)
    in fmap f (load_tuning_scl nm)

load_tuning_d12 :: (String,Double,Int) -> IO T.Sparse_Midi_Tuning_F
load_tuning_d12 (nm,dt,k) =
    let f tn = fmap Just (T.d12_midi_tuning_f (tn,dt,k))
    in fmap f (load_tuning_scl nm)

load_tuning_tbl :: (String,Double,Int) -> IO T.Sparse_Midi_Tuning_F
load_tuning_tbl (nm,dt,k) =
    let from_cps = T.cps_to_midi_detune . flip T.cps_shift_cents dt
        f tbl mnn = fmap from_cps (lookup (mnn + k) tbl)
    in fmap f (load_cps_tbl nm)

load_tuning_ty :: String -> (String,Double,Int) -> IO T.Sparse_Midi_Tuning_F
load_tuning_ty ty opt =
    case ty of
      "cps" -> load_tuning_cps opt
      "d12" -> load_tuning_d12 opt
      "tbl" -> load_tuning_tbl opt
      _ -> error "cps|d12|tbl"

