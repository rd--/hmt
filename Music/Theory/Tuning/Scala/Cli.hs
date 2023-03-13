-- | Command line interface to hmt/scala.
module Music.Theory.Tuning.Scala.Cli where

import Data.Char {- base -}
import Data.List {- base -}
import System.Environment {- base -}
import Text.Printf {- base -}

import qualified Music.Theory.Array.Text as T {- hmt-base -}
import qualified Music.Theory.Function as T {- hmt-base -}
import qualified Music.Theory.List as T {- hmt-base -}
import qualified Music.Theory.Read as T {- hmt-base -}
import qualified Music.Theory.Show as T {- hmt-base -}

import qualified Music.Theory.Array.Csv.Midi.Mnd as T {- hmt -}
import qualified Music.Theory.Pitch as T {- hmt -}
import qualified Music.Theory.Pitch.Spelling.Table as T {- hmt -}
import qualified Music.Theory.Time.Seq as T {- hmt -}
import qualified Music.Theory.Tuning as T {- hmt -}
import qualified Music.Theory.Tuning.Et as T {- hmt -}
import qualified Music.Theory.Tuning.Midi as T {- hmt -}
import qualified Music.Theory.Tuning.Scala as Scala {- hmt -}
import qualified Music.Theory.Tuning.Scala.Kbm as Kbm {- hmt -}
import qualified Music.Theory.Tuning.Scala.Functions as Functions {- hmt -}
import qualified Music.Theory.Tuning.Scala.Interval as Interval {- hmt -}
import qualified Music.Theory.Tuning.Scala.Mode as Mode {- hmt -}
import qualified Music.Theory.Tuning.Type as T {- hmt -}

type R = Double

db_stat :: IO ()
db_stat = do
  db <- Scala.scl_load_db_path
  let po = filter (== Just (Right 2)) (map Scala.scale_octave db)
      uf = filter Scala.is_scale_uniform db
      r = ["# entries        : " ++ show (length db)
          ,"# perfect-octave : " ++ show (length po)
          ,"# scale-uniform  : " ++ show (length uf)]
  putStrLn (unlines r)

-- > db_summarise (Just 15) (Just 65)
db_summarise :: Maybe Int -> Maybe Int -> IO ()
db_summarise nm_lim dsc_lim = do
  db <- Scala.scl_load_db_path
  let nm_seq = map Scala.scale_name db
      nm_max = maybe (maximum (map length nm_seq)) id nm_lim
      dsc_seq = map Scala.scale_description db
      fmt (nm,dsc) = printf "%-*s : %s" nm_max (take nm_max nm) (maybe dsc (flip take dsc) dsc_lim)
      tbl = map fmt (zip nm_seq dsc_seq)
  putStrLn (unlines tbl)

env :: IO ()
env = do
  scl_dir <- Scala.scl_get_dir
  scl_path <- Scala.scl_get_path
  dist_dir <- getEnv "SCALA_DIST_DIR"
  putStrLn ("SCALA_SCL_DIR = " ++ if null scl_dir then "NOT SET" else scl_dir)
  putStrLn ("SCALA_SCL_PATH = " ++ if null scl_path then "NOT SET" else intercalate ":" scl_path)
  putStrLn ("SCALA_DIST_DIR = " ++ if null dist_dir then "NOT SET" else dist_dir)

cut :: Maybe Int -> [a] -> [a]
cut lm s = maybe s (\n -> take n s) lm

search :: (IO [a], a -> String, a -> [String]) -> (Bool, Maybe Int) -> [String] -> IO ()
search (load_f,descr_f,stat_f) (ci,lm) txt = do
  db <- load_f
  let modify = if ci then map toLower else id
      txt' = map modify txt
      db' = filter (T.predicate_all (map isInfixOf txt') . modify . descr_f) db
  mapM_ (putStrLn . unlines . map (cut lm) . stat_f) db'

-- > search_scale (True,Nothing) ["xenakis"]
-- > search_scale (True,Just 75) ["lamonte","young"]
search_scale :: (Bool,Maybe Int) -> [String] -> IO ()
search_scale = search (Scala.scl_load_db_path,Scala.scale_description,Scala.scale_stat)

-- > search_mode (True,Nothing) ["xenakis"]
search_mode :: (Bool,Maybe Int) -> [String] -> IO ()
search_mode = search (fmap Mode.modenam_modes Mode.load_modenam,Mode.mode_description,Mode.mode_stat)

-- > stat_all Nothing
stat_all :: Maybe Int -> IO ()
stat_all character_limit = do
  db <- Scala.scl_load_db_path
  mapM_ (putStrLn . unlines . map (cut character_limit) . Scala.scale_stat) db

-- > stat_by_name Nothing "young-lm_piano"
stat_by_name :: Maybe Int -> FilePath -> IO ()
stat_by_name lm nm = do
  sc <- Scala.scl_load nm
  putStrLn (unlines (map (cut lm) (Scala.scale_stat sc)))

-- > rng_enum (60,72) == [60 .. 72]
rng_enum :: Enum t => (t,t) -> [t]
rng_enum (l,r) = [l .. r]

cps_tbl :: String -> T.Mnn_Cps_Table -> (T.Midi,T.Midi) -> IO ()
cps_tbl fmt tbl mnn_rng = do
  let cps_pp = T.double_pp 2
      cents_pp = T.double_pp 1
      gen_t i = (i,T.midi_to_pitch_ks i,T.lookup_err i tbl)
      t_pp (i,p,cps) =
          let ref = T.midi_to_cps i
              (_,nr,nr_cps,_,_) = T.nearest_12et_tone_k0 (69,440) cps
          in [show i
             ,cps_pp cps,T.pitch_pp_iso nr,cents_pp (T.cps_difference_cents nr_cps cps)
             ,cps_pp ref,T.pitch_pp_iso p,cents_pp (T.cps_difference_cents ref cps)]
      hdr = ["MNN"
            ,"CPS","ET12","CENTS-/+"
            ,"REF CPS","REF ET12","CENTS-/+"]
      dat = map (t_pp . gen_t) (rng_enum mnn_rng)
      ln = case fmt of
             "md" -> T.table_pp T.table_opt_simple (hdr : dat)
             "csv" -> map (intercalate ",") dat
             _ -> error "cps_tbl: fmt?"
  putStr (unlines ln)

-- > cps_tbl_d12 "md" ("young-lm_piano",-74.7,-3) (60,72)
cps_tbl_d12 :: String -> (String,T.Cents,T.Midi) -> (T.Midi,T.Midi) -> IO ()
cps_tbl_d12 fmt (nm,c,k) mnn_rng = do
  t <- Scala.scl_load_tuning nm :: IO T.Tuning
  let tbl = T.gen_cps_tuning_tbl (T.lift_tuning_f (T.d12_midi_tuning_f (t,c,k)))
  cps_tbl fmt tbl mnn_rng

-- > cps_tbl_cps "md" ("cet111",27.5,9,127-9) (69,69+25)
cps_tbl_cps :: String -> (String,R,T.Midi,Int) -> (T.Midi,T.Midi) -> IO ()
cps_tbl_cps fmt (nm,f0,k,n) mnn_rng = do
  t <- Scala.scl_load_tuning nm
  let tbl = T.gen_cps_tuning_tbl (T.cps_midi_tuning_f (t,f0,k,n))
  cps_tbl fmt tbl mnn_rng

csv_mnd_retune_d12 :: (String,T.Cents,T.Midi) -> FilePath -> FilePath -> IO ()
csv_mnd_retune_d12 (nm,c,k) in_fn out_fn = do
  t <- Scala.scl_load_tuning nm
  let retune_f = T.midi_detune_to_fmidi . T.d12_midi_tuning_f (t,c,k)
  m <- T.csv_midi_read_wseq in_fn :: IO (T.Wseq R (R,R,T.Channel,T.Param))
  let f (tm,(mnn,vel,ch,pm)) = (tm,(retune_f (floor mnn),vel,ch,pm))
  T.csv_mndd_write_wseq 4 out_fn (map f m)

-- > fluidsynth_tuning_d12 ("young-lm_piano",0,0) ("young-lm_piano",-74.7,-3)
fluidsynth_tuning_d12 :: (String,Int,Int) -> (String,T.Cents,T.Midi) -> IO ()
fluidsynth_tuning_d12 (fs_name,fs_bank,fs_prog) (nm,c,k) = do
  t <- Scala.scl_load_tuning nm :: IO T.Tuning
  let tun_f = T.d12_midi_tuning_f (t,c,k)
      pp_f n = let (mnn,dt) = tun_f n
                   cents = fromIntegral mnn * 100 + dt
                   cents_non_neg = if cents < 0 then 0 else cents
               in printf "tune %d %d %d %.2f" fs_bank fs_prog n cents_non_neg
      l = printf "tuning \"%s\" %d %d" fs_name fs_bank fs_prog : map pp_f [0 .. 127]
  putStrLn (unlines l)

{-
import Data.Int {- base -}
import Data.Word {- base -}

int_to_int8 :: Int -> Int8
int_to_int8 = fromIntegral

int8_to_word8 :: Int8 -> Word8
int8_to_word8 = fromIntegral

midi_tbl_binary_mnn_cents_tuning_d12 :: FilePath -> (String,T.Cents,Int) -> IO ()
midi_tbl_binary_mnn_cents_tuning_d12 fn (nm,c,k) = do
  t <- Scala.scl_load_tuning nm :: IO T.Tuning
  let tun_f = T.d12_midi_tuning_f (t,c,k)
      pp_f n = let (mnn,dt) = T.midi_detune_normalise (tun_f n)
               in [int_to_int8 mnn,int_to_int8 (round dt)]
  B.writeFile fn (B.pack (map int8_to_word8 (concatMap pp_f [0 .. 127])))
-}

{-
> midi_tbl_tuning_d12 "freq" ("meanquar",0,0)
> midi_tbl_tuning_d12 "fmidi" ("meanquar",0,0)
> midi_tbl_tuning_d12 "mts" ("young-lm_piano",-74.7,-3)
-}
midi_tbl_tuning_d12 :: String -> (String,T.Cents,T.Midi) -> IO ()
midi_tbl_tuning_d12 typ (nm,c,k) = do
  t <- Scala.scl_load_tuning nm :: IO T.Tuning
  let tun_f = T.d12_midi_tuning_f (t,c,k)
      pp_f n =
        case typ of
          "fmidi" -> printf "%3d,%10.6f" n (T.midi_detune_to_fmidi (tun_f n))
          "freq" -> printf "%3d,%10.4f" n (T.midi_detune_to_cps (tun_f n))
          "mts" ->
            let (mnn,dt) = T.midi_detune_normalise_positive (tun_f n)
            in printf "%3d,%3d,%7.4f" n (mnn `mod` 0x80) dt
          _ -> error "midi_tbl_tuning_d12"
  putStr (unlines (map pp_f [0 .. 127]))

ratio_cents_pp :: Rational -> String
ratio_cents_pp = show . (round :: Double -> Int) . T.ratio_to_cents

-- > intnam_lookup [7/4,7/6,9/8,13/8]
intnam_lookup :: [Rational] -> IO ()
intnam_lookup r_sq = do
  let f db r = let nm = maybe "*Unknown*" snd (Interval.intnam_search_ratio db r)
               in concat [T.ratio_pp r," = ",nm," = ",ratio_cents_pp r]
  db <- Interval.load_intnam
  mapM_ (putStrLn . f db) r_sq

-- > intnam_search "didymus"
intnam_search :: String -> IO ()
intnam_search txt = do
  db <- Interval.load_intnam
  let f (r,nm) = concat [T.ratio_pp r," = ",nm," = ",ratio_cents_pp r]
  mapM_ (putStrLn . f) (Interval.intnam_search_description_ci db txt)

kbm_tbl :: String -> String -> String -> IO ()
kbm_tbl ty scl_nm kbm_nm = do
  scl <- Scala.scl_load scl_nm
  kbm <- Kbm.kbm_load kbm_nm
  let tbl = case ty of
        "cps" -> Kbm.kbm_cps_tbl kbm scl
        "fmidi" -> Kbm.kbm_fmidi_tbl kbm scl
        _ -> error "kbm_tbl: unknown type"
      fmt (i,j) = printf "%d,%.4f" i j
      txt = unlines (map fmt tbl)
  putStrLn txt

-- * Main

help :: [String]
help =
    ["cps-tbl md|csv cps name:string f0:real mnn0:int gamut:int mnn-l:int mnn-r:int"
    ,"cps-tbl md|csv d12 name:string cents:real mnn:int mnn-l:int mnn-r:int"
    ,"csv-mnd-retune d12 name:string cents:real mnn:int input-file output-file"
    ,"db stat"
    ,"db summarise nm-lm|nil dsc-lm|nil"
    ,"env"
    ,"fluidsynth d12 scl-name:string cents:real mnn:int fs-name:string fs-bank:int fs-prog:int"
    ,"intervals {half-matrix|list|matrix} {cents|ratios} scale-name:string"
    ,"intname lookup interval:rational..."
    ,"intname search text:string"
    ,"kbm table {cps | fmidi} scala-name:string kbm-name:string"
    ,"midi-table fmidi|freq|mts d12 name:string cents:real mnn:int"
    ,"search scale|mode ci|cs lm|nil text:string..."
    ,"stat all lm|nil"
    ,"stat scale lm|nil name:string|file-path"
    ,""
    ,"  lm:int = line character limit"]

nil_or_read :: Read a => String -> Maybe a
nil_or_read s = if s == "nil" then Nothing else Just (T.read_err s)

scala_cli :: [String] -> IO ()
scala_cli arg = do
  let usage = putStrLn (unlines help)
  case arg of
    ["cps-tbl",fmt,"cps",nm,f0,k,n,l,r] -> cps_tbl_cps fmt (nm,read f0,read k,read n) (read l,read r)
    ["cps-tbl",fmt,"d12",nm,c,k,l,r] -> cps_tbl_d12 fmt (nm,read c,read k) (read l,read r)
    ["csv-mnd-retune","d12",nm,c,k,in_fn,out_fn] -> csv_mnd_retune_d12 (nm,read c,read k) in_fn out_fn
    ["db","stat"] -> db_stat
    ["db","summarise",nm_lim,dsc_lim] -> db_summarise (nil_or_read nm_lim) (nil_or_read dsc_lim)
    ["env"] -> env
    ["fluidsynth","d12",scl_nm,c,k,fs_nm,fs_bank,fs_prog] ->
        fluidsynth_tuning_d12 (fs_nm,read fs_bank,read fs_prog) (scl_nm,read c,read k)
    ["intervals","half-matrix",'c':_,k,nm] -> Functions.intervals_half_matrix_cents (read k) nm
    ["intervals","half-matrix",'r':_,nm] -> Functions.intervals_half_matrix_ratios nm
    ["intervals","list",'r':_,nm] -> Functions.intervals_list_ratios nm
    ["intervals","matrix",'c':_,k,nm] -> Functions.intervals_matrix_cents (read k) nm
    ["intervals","matrix",'r':_,nm] -> Functions.intervals_matrix_ratios nm
    "intnam":"lookup":r_sq -> intnam_lookup (map (T.read_ratio_with_div_err False) r_sq)
    ["intnam","search",txt] -> intnam_search txt
    ["kbm","table",ty,scl_nm,kbm_nm] -> kbm_tbl ty scl_nm kbm_nm
    ["midi-table",typ,"d12",scl_nm,c,k] -> midi_tbl_tuning_d12 typ (scl_nm,read c,read k)
    "search":ty:ci:lm:txt ->
        case ty of
          "scale" -> search_scale (ci == "ci",nil_or_read lm) txt
          "mode" -> search_mode (ci == "ci",nil_or_read lm) txt
          _ -> usage
    ["stat","all",lm] -> stat_all (nil_or_read lm)
    ["stat","scale",lm,nm] -> stat_by_name (nil_or_read lm) nm
    _ -> usage
