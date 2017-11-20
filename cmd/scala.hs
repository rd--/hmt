import qualified Data.ByteString as B {- bytestring -}
import Data.Char {- base -}
import Data.Int {- base -}
import Data.List {- base -}
import Data.Word {- base -}
import System.Environment {- base -}
import Text.Printf {- base -}

import qualified Music.Theory.Array.CSV.Midi.MND as T {- hmt -}
import qualified Music.Theory.Array.MD as T {- hmt -}
import qualified Music.Theory.Function as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Math as T {- hmt -}
import qualified Music.Theory.Pitch as T {- hmt -}
import qualified Music.Theory.Pitch.Spelling.Table as T {- hmt -}
import qualified Music.Theory.Read as T {- hmt -}
import qualified Music.Theory.Time.Seq as T {- hmt -}
import qualified Music.Theory.Tuning as T {- hmt -}
import qualified Music.Theory.Tuning.ET as T {- hmt -}
import qualified Music.Theory.Tuning.Scala as T {- hmt -}
import qualified Music.Theory.Tuning.Scala.Interval as T {- hmt -}
import qualified Music.Theory.Tuning.Scala.Mode as T {- hmt -}

type R = Double

db_stat :: IO ()
db_stat = do
  db <- T.scl_load_db :: IO [T.Scale Integer]
  let po = filter (== Just (Right 2)) (map T.scale_octave db)
      uf = filter T.is_scale_uniform db
      r = ["# entries        : " ++ show (length db)
          ,"# perfect-octave : " ++ show (length po)
          ,"# scale-uniform  : " ++ show (length uf)]
  putStrLn (unlines r)

env :: IO ()
env = do
  scl_dir <- T.scl_get_dir
  dist_dir <- getEnv "SCALA_DIST_DIR"
  putStrLn ("SCALA_SCL_DIR = " ++ if null scl_dir then "NOT SET" else intercalate ":" scl_dir)
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
search_scale = search (T.scl_load_db :: IO [T.Scale Integer],T.scale_description,T.scale_stat)

-- > search_mode (True,Nothing) ["xenakis"]
search_mode :: (Bool,Maybe Int) -> [String] -> IO ()
search_mode = search (fmap T.modenam_modes T.load_modenam,T.mode_description,T.mode_stat)

stat_all :: Maybe Int -> IO ()
stat_all lm = do
  db <- T.scl_load_db :: IO [T.Scale Integer]
  mapM_ (putStrLn . unlines . map (cut lm) . T.scale_stat) db

-- > stat_by_name Nothing "young-lm_piano"
stat_by_name :: Maybe Int -> FilePath -> IO ()
stat_by_name lm nm = do
  sc <- T.scl_load nm :: IO (T.Scale Integer)
  putStrLn (unlines (map (cut lm) (T.scale_stat sc)))

-- > rng_enum (60,72) == [60 .. 72]
rng_enum :: Enum t => (t,t) -> [t]
rng_enum (l,r) = [l .. r]

cps_tbl :: String -> T.MNN_CPS_Table -> (Int,Int) -> IO ()
cps_tbl fmt tbl mnn_rng = do
  let cps_pp = T.double_pp 2
      cents_pp = T.double_pp 1
      gen_t i = (i,T.midi_to_pitch_ks i,T.lookup_err i tbl)
      t_pp (i,p,cps) =
          let ref = T.midi_to_cps i
              (_,nr,nr_cps,_,_) = T.nearest_12et_tone cps
          in [show i
             ,cps_pp cps,T.pitch_pp_iso nr,cents_pp (T.cps_difference_cents nr_cps cps)
             ,cps_pp ref,T.pitch_pp_iso p,cents_pp (T.cps_difference_cents ref cps)]
      hdr = Just ["MNN"
                 ,"CPS","ET12","CENTS-/+"
                 ,"REF CPS","REF ET12","CENTS-/+"]
      dat = map (t_pp . gen_t) (rng_enum mnn_rng)
      ln = case fmt of
             "md" -> T.md_table hdr dat
             "csv" -> map (intercalate ",") dat
             _ -> error "cps_tbl: fmt?"
  putStr (unlines ln)

-- > cps_tbl_d12 "md" ("young-lm_piano",-74.7,-3) (60,72)
cps_tbl_d12 :: String -> (String,T.Cents,Int) -> (Int,Int) -> IO ()
cps_tbl_d12 fmt (nm,c,k) mnn_rng = do
  t <- T.scl_load_tuning 0.01 nm :: IO T.Tuning
  let tbl = T.gen_cps_tuning_tbl (T.lift_tuning_f (T.d12_midi_tuning_f (t,c,k)))
  cps_tbl fmt tbl mnn_rng

-- > cps_tbl_cps "md" ("cet111",27.5,9,127-9) (69,69+25)
cps_tbl_cps :: String -> (String,R,Int,Int) -> (Int,Int) -> IO ()
cps_tbl_cps fmt (nm,f0,k,n) mnn_rng = do
  t <- T.scl_load_tuning 0.01 nm
  let tbl = T.gen_cps_tuning_tbl (T.cps_midi_tuning_f (t,f0,k,n))
  cps_tbl fmt tbl mnn_rng

csv_mnd_retune_d12 :: (String,T.Cents,Int) -> FilePath -> FilePath -> IO ()
csv_mnd_retune_d12 (nm,c,k) in_fn out_fn = do
  t <- T.scl_load_tuning 0.01 nm
  let retune_f = T.midi_detune_to_fmidi . T.d12_midi_tuning_f (t,c,k)
  m <- T.csv_midi_read_wseq in_fn :: IO (T.Wseq R (R,R,T.Channel,[T.Param]))
  let f (tm,(mnn,vel,ch,pm)) = (tm,(retune_f (floor mnn),vel,ch,pm))
  T.csv_mndd_write_wseq 4 out_fn (map f m)

-- > fluidsynth_tuning_d12 ("young-lm_piano",0,0) ("young-lm_piano",-74.7,-3)
fluidsynth_tuning_d12 :: (String,Int,Int) -> (String,T.Cents,Int) -> IO ()
fluidsynth_tuning_d12 (fs_name,fs_bank,fs_prog) (nm,c,k) = do
  t <- T.scl_load_tuning 0.01 nm :: IO T.Tuning
  let tun_f = T.d12_midi_tuning_f (t,c,k)
      pp_f n = let (mnn,dt) = tun_f n
                   cents = fromIntegral mnn * 100 + dt
                   cents' = if cents < 0 then 0 else cents
               in printf "tune %d %d %d %.2f" fs_bank fs_prog n cents'
      l = printf "tuning \"%s\" %d %d" fs_name fs_bank fs_prog : map pp_f [0 .. 127]
  putStrLn (unlines l)

int_to_int8 :: Int -> Int8
int_to_int8 = fromIntegral

int8_to_word8 :: Int8 -> Word8
int8_to_word8 = fromIntegral

-- > midi_tbl_binary_mnn_cents_tuning_d12 "/home/rohan/data/dexed/meanquar.bin" ("meanquar",0,0)
-- > midi_tbl_binary_mnn_cents_tuning_d12 "/home/rohan/data/dexed/young-lm_piano.bin" ("young-lm_piano",-74.7,-3)
midi_tbl_binary_mnn_cents_tuning_d12 :: FilePath -> (String,T.Cents,Int) -> IO ()
midi_tbl_binary_mnn_cents_tuning_d12 fn (nm,c,k) = do
  t <- T.scl_load_tuning 0.01 nm :: IO T.Tuning
  let tun_f = T.d12_midi_tuning_f (t,c,k)
      pp_f n = let (mnn,dt) = T.midi_detune_normalise (tun_f n)
               in [int_to_int8 mnn,int_to_int8 (round dt)]
  B.writeFile fn (B.pack (map int8_to_word8 (concatMap pp_f [0 .. 127])))

ratio_cents_pp :: Rational -> String
ratio_cents_pp = show . (round :: Double -> Int) . T.ratio_to_cents

-- > intnam_lookup [7/4,7/6,9/8,13/8]
intnam_lookup :: [Rational] -> IO ()
intnam_lookup r_sq = do
  let f db r = let nm = maybe "*UNKNOWN*" snd (T.intnam_search_ratio db r)
               in concat [T.ratio_pp r," = ",nm," = ",ratio_cents_pp r]
  db <- T.load_intnam
  mapM_ (putStrLn . f db) r_sq

-- > intnam_search "didymus"
intnam_search :: String -> IO ()
intnam_search txt = do
  db <- T.load_intnam
  let f (r,nm) = concat [T.ratio_pp r," = ",nm," = ",ratio_cents_pp r]
  mapM_ (putStrLn . f) (T.intnam_search_description_ci db txt)

help :: [String]
help =
    ["cps-tbl md|csv cps name:string f0:real mnn0:int gamut:int mnn-l:int mnn-r:int"
    ,"cps-tbl md|csv d12 name:string cents:real mnn:int mnn-l:int mnn-r:int"
    ,"csv-mnd-retune d12 name:string cents:real mnn:int input-file output-file"
    ,"db-stat"
    ,"env"
    ,"fluidsynth d12 scl-name:string cents:real mnn:int fs-name:string fs-bank:int fs-prog:int"
    ,"intname lookup interval:rational..."
    ,"intname search text:string"
    ,"midi-table binary-mnn-cents d12 name:string cents:real mnn:int output-file"
    ,"search scale|mode ci|cs lm|nil text:string..."
    ,"stat all lm|nil"
    ,"stat scale lm|nil name:string|file-path"
    ,""
    ,"  lm:int = line character limit"]

nil_or_read :: Read a => String -> Maybe a
nil_or_read s = if s == "nil" then Nothing else Just (T.read_err s)

main :: IO ()
main = do
  a <- getArgs
  let usage = putStrLn (unlines help)
  case a of
    ["cps-tbl",fmt,"cps",nm,f0,k,n,l,r] ->
        cps_tbl_cps fmt (nm,read f0,read k,read n) (read l,read r)
    ["cps-tbl",fmt,"d12",nm,c,k,l,r] ->
        cps_tbl_d12 fmt (nm,read c,read k) (read l,read r)
    ["csv-mnd-retune","d12",nm,c,k,in_fn,out_fn] ->
        csv_mnd_retune_d12 (nm,read c,read k) in_fn out_fn
    ["db-stat"] ->
        db_stat
    ["env"] ->
        env
    ["fluidsynth","d12",scl_nm,c,k,fs_nm,fs_bank,fs_prog] ->
        fluidsynth_tuning_d12 (fs_nm,read fs_bank,read fs_prog) (scl_nm,read c,read k)
    "intnam":"lookup":r_sq ->
        intnam_lookup (map T.read_ratio_with_div_err r_sq)
    ["intnam","search",txt] ->
        intnam_search txt
    ["midi-table","binary-mnn-cents","d12",scl_nm,c,k,fn] ->
        midi_tbl_binary_mnn_cents_tuning_d12 fn (scl_nm,read c,read k)
    "search":ty:ci:lm:txt ->
        case ty of
          "scale" -> search_scale (ci == "ci",nil_or_read lm) txt
          "mode" -> search_mode (ci == "ci",nil_or_read lm) txt
          _ -> usage
    ["stat","all",lm] ->
        stat_all (nil_or_read lm)
    ["stat","scale",lm,nm] ->
        stat_by_name (nil_or_read lm) nm
    _ -> usage
