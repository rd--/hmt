import Data.Char {- base -}
import Data.List {- base -}
import System.Environment {- base -}

import qualified Music.Theory.Array.CSV.Midi.MND as T {- hmt -}
import qualified Music.Theory.Array.MD as T {- hmt -}
import qualified Music.Theory.Function as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Math as T {- hmt -}
import qualified Music.Theory.Pitch as T {- hmt -}
import qualified Music.Theory.Read as T {- hmt -}
import qualified Music.Theory.Tuning as T {- hmt -}
import qualified Music.Theory.Tuning.ET as T {- hmt -}
import qualified Music.Theory.Tuning.Scala as T {- hmt -}
import qualified Music.Theory.Tuning.Scala.Mode as T {- hmt -}

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
  dir <- T.scl_get_dir
  putStrLn ("SCALA_SCL_DIR = " ++ if null dir then "NOT SET" else intercalate ":" dir)

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

cps_tbl :: T.MNN_CPS_Table -> (Int,Int) -> IO ()
cps_tbl tbl (l,r) = do
  let cps_pp = T.double_pp 2
      gen_t i = (i,T.midi_to_pitch_ks i,T.lookup_err i tbl)
      t_pp (i,p,cps) = let ref = T.midi_to_cps i
                           (_,nr,_,_,_) = T.nearest_12et_tone cps
                       in [show i,T.pitch_pp_iso p,cps_pp cps,T.pitch_pp_iso nr
                          ,cps_pp ref,cps_pp (cps - ref)]
      hdr = Just ["MNN","PITCH","CPS","NEAR","ET12","-/+"]
  putStr (unlines (T.md_table hdr (map (t_pp . gen_t) [l .. r])))

-- > cps_tbl_d12 ("young-lm_piano",-74.7,3) (60,72)
cps_tbl_d12 :: (String,T.Cents,Int) -> (Int,Int) -> IO ()
cps_tbl_d12 (nm,c,k) (l,r) = do
  t <- T.scl_load_tuning 0.01 nm :: IO T.Tuning
  let tbl = T.gen_cps_tuning_tbl (T.d12_midi_tuning_f (t,c,k))
  cps_tbl tbl (l,r)

-- > cps_tbl_cps ("cet111",27.5,9,127-9) (69,69+25)
cps_tbl_cps :: (String,Double,Int,Int) -> (Int,Int) -> IO ()
cps_tbl_cps (nm,f0,k,n) (l,r) = do
  t <- T.scl_load_tuning 0.01 nm
  let tbl = T.gen_cps_tuning_tbl (T.cps_midi_tuning_f (t,f0,k,n))
  cps_tbl tbl (l,r)

csv_mnd_retune_d12 :: (String,T.Cents,Int) -> FilePath -> FilePath -> IO ()
csv_mnd_retune_d12 (nm,c,k) in_fn out_fn = do
  t <- T.scl_load_tuning 0.01 nm
  let retune_f = T.midi_detune_to_fmidi . T.d12_midi_tuning_f (t,c,k)
  m <- T.csv_mnd_read in_fn :: IO [T.MND Double Int]
  let f (tm,ty,mnn,vel,ch) = (tm,ty,retune_f mnn,fromIntegral vel,ch)
  T.csv_mnd_write 4 out_fn (map f m)

help :: [String]
help =
    ["cps-tbl cps name:string f0:double mnn0:int gamut:int mnn-l:int mnn-r:int"
    ,"cps-tbl d12 name:string cents:double mnn:int mnn-l:int mnn-r:int"
    ,"csv-mnd-retune d12 name:string cents:double mnn:int input-file output-file"
    ,"db-stat"
    ,"env"
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
    ["cps-tbl","cps",nm,f0,k,n,l,r] -> cps_tbl_cps (nm,read f0,read k,read n) (read l,read r)
    ["cps-tbl","d12",nm,c,k,l,r] -> cps_tbl_d12 (nm,read c,read k) (read l,read r)
    ["csv-mnd-retune","d12",nm,c,k,in_fn,out_fn] -> csv_mnd_retune_d12 (nm,read c,read k) in_fn out_fn
    ["db-stat"] -> db_stat
    ["env"] -> env
    "search":ty:ci:lm:txt ->
        case ty of
          "scale" -> search_scale (ci == "ci",nil_or_read lm) txt
          "mode" -> search_mode (ci == "ci",nil_or_read lm) txt
          _ -> usage
    ["stat","all",lm] -> stat_all (nil_or_read lm)
    ["stat","scale",lm,nm] -> stat_by_name (nil_or_read lm) nm
    _ -> usage
