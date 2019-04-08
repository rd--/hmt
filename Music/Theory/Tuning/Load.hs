-- | Functions to load a tuning definition and transform it into a sparse tuning function.
module Music.Theory.Tuning.Load where

import System.Random {- random -}

import qualified Music.Theory.Array.CSV as T
import qualified Music.Theory.Pitch as T
import qualified Music.Theory.Tuning as T
import qualified Music.Theory.Tuning.Midi as T
import qualified Music.Theory.Tuning.Scala as T

-- | Load possibly sparse and possibly one-to-many
-- (midi-note-number,cps-frequency) table from CSV file.
--
-- > load_cps_tbl "/home/rohan/dr.csv"
load_cps_tbl :: FilePath -> IO [(Int,Double)]
load_cps_tbl nm = do
  tbl <- T.csv_table_read_def id nm
  let f e = case e of
              [p,q] -> (read p,read q)
              _ -> error "load_cps_tbl"
  return (map f tbl)

-- | Load scala scl file as 'T.Tuning'.
load_tuning_scl :: String -> IO T.Tuning
load_tuning_scl = fmap (T.scale_to_tuning 0.01) . T.scl_load

-- | cps = (tuning-name,frequency-zero,midi-note-number-of-f0)
--   d12 = (tuning-name,cents-deviation,midi-note-offset)
type LOAD_TUNING_OPT = (String,Double,Int)

-- | Load scala file and apply 'T.cps_midi_tuning_f'.
load_tuning_cps :: LOAD_TUNING_OPT -> IO T.Sparse_Midi_Tuning_F
load_tuning_cps (nm,f0,k) =
    let f tn = T.cps_midi_tuning_f (tn,f0,k,128-k)
    in fmap f (load_tuning_scl nm)

-- | Load scala file and apply 'T.d12_midi_tuning_f'.
load_tuning_d12 :: LOAD_TUNING_OPT -> IO T.Sparse_Midi_Tuning_F
load_tuning_d12 (nm,dt,k) =
    let f tn = T.lift_tuning_f (T.d12_midi_tuning_f (tn,dt,k))
    in fmap f (load_tuning_scl nm)

-- | Lookup first matching element in table.
load_tuning_tbl :: LOAD_TUNING_OPT -> IO T.Sparse_Midi_Tuning_F
load_tuning_tbl (nm,dt,k) =
    let from_cps = T.cps_to_midi_detune . flip T.cps_shift_cents dt
        f tbl mnn = fmap from_cps (lookup (mnn + k) tbl)
    in fmap f (load_cps_tbl nm)

type Choose_f st t = [t] -> st-> (t,st)

-- | Randomly choose from elements in table, equal weighting.
default_choose_f :: RandomGen g => Choose_f g t
default_choose_f l g =
    let (i,g') = randomR (0,length l - 1) g
    in (l !! i,g')

-- | Load tuning table with stateful selection function for one-to-many entries.
load_tuning_tbl_st :: Choose_f st (Int,Double) -> LOAD_TUNING_OPT -> IO (T.Sparse_Midi_Tuning_ST_F st)
load_tuning_tbl_st choose_f (nm,dt,k) =
    let from_cps = T.cps_to_midi_detune . flip T.cps_shift_cents dt
        f tbl g mnn = case filter ((== (mnn + k)) . fst) tbl of
                        [] -> (g,Nothing)
                        l -> let ((_,e),g') = choose_f l g
                             in (g',Just (from_cps e))
    in fmap f (load_cps_tbl nm)

load_tuning_ty :: String -> LOAD_TUNING_OPT -> IO T.Sparse_Midi_Tuning_F
load_tuning_ty ty opt =
    case ty of
      "cps" -> load_tuning_cps opt
      "d12" -> load_tuning_d12 opt
      "tbl" -> load_tuning_tbl opt
      _ -> error "cps|d12|tbl"

load_tuning_st_ty :: String -> LOAD_TUNING_OPT -> IO (T.Sparse_Midi_Tuning_ST_F StdGen)
load_tuning_st_ty ty opt =
    case ty of
      "cps" -> fmap T.lift_sparse_tuning_f (load_tuning_cps opt)
      "d12" -> fmap T.lift_sparse_tuning_f (load_tuning_d12 opt)
      "tbl" -> load_tuning_tbl_st default_choose_f opt
      _ -> error "cps|d12|tbl"
