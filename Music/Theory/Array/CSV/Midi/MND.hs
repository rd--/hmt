-- | Functions for reading midi note data (MND) from CSV files.
-- This is /not/ a generic text midi notation.
-- The defined commands are @on@ and @off@, but others may be present.
-- Non-integral note number and key velocity data are allowed.
module Music.Theory.Array.CSV.Midi.MND where

import Data.List.Split {- split -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Word {- base -}

import qualified Music.Theory.Array.CSV as T {- hmt -}
import qualified Music.Theory.Math as T {- hmt -}
import qualified Music.Theory.Read as T {- hmt -}
import qualified Music.Theory.Time.Seq as T {- hmt -}

-- | If /r/ is whole to /k/ places then show as integer, else as float to /k/ places.
data_value_pp :: Real t => Int -> t -> String
data_value_pp k r =
    if T.whole_to_precision k r
    then show (T.real_floor_int r)
    else T.real_pp k r

-- | Channel values are 4-bit (0-15).
type Channel = Word8

-- | The required header field.
csv_mnd_hdr :: [String]
csv_mnd_hdr = ["time","on/off","note","velocity","channel","param"]

type Param = (String,Double)

param_parse :: String -> [Param]
param_parse str =
    let f x = case splitOn "=" x of
                [lhs,rhs] -> (lhs,read rhs)
                _ -> error ("param_parse: " ++ x)
    in if null str then [] else map f (splitOn ";" str)

param_pp :: Int -> [Param] -> String
param_pp k =
    let f (lhs,rhs) = concat [lhs,"=",T.real_pp k rhs]
    in intercalate ";" . map f

-- | Midi note data, the type parameters are to allow for fractional note & velocity values.
-- The command is a string, @on@ and @off@ are standard, other commands may be present.
--
-- > unwords csv_mnd_hdr == "time on/off note velocity channel param"
type MND t n = (t,String,n,n,Channel,[Param])

csv_mnd_parse :: (Read t,Real t,Read n,Real n) => T.CSV_Table String -> [MND t n]
csv_mnd_parse (hdr,dat) =
    let err x = error ("csv_mnd_read: " ++ x)
        f m = case m of
                [st,msg,mnn,vel,ch,pm] ->
                    (T.reads_exact_err "time:real" st
                    ,msg
                    ,T.reads_exact_err "note:real" mnn
                    ,T.reads_exact_err "velocity:real" vel
                    ,T.reads_exact_err "channel:int" ch
                    ,param_parse pm)
                _ -> err "entry?"
    in case hdr of
         Just hdr' -> if hdr' == csv_mnd_hdr then map f dat else err "header?"
         Nothing -> err "no header?"

load_csv :: FilePath -> IO (T.CSV_Table String)
load_csv = T.csv_table_read (True,',',False,T.CSV_No_Align) id

-- | Midi note data.
--
-- > let fn = "/home/rohan/cvs/uc/uc-26/daily-practice/2014-08-13.1.csv"
-- > m <- csv_mnd_read fn :: IO [MND Double Double]
-- > length m == 17655
-- > csv_mnd_write 4 "/tmp/t.csv" m
csv_mnd_read :: (Read t,Real t,Read n,Real n) => FilePath -> IO [MND t n]
csv_mnd_read = fmap csv_mnd_parse . load_csv

-- | Writer.
csv_mnd_write :: (Real t,Real n) => Int -> FilePath -> [MND t n] -> IO ()
csv_mnd_write r_prec nm =
    let un_node (st,msg,mnn,vel,ch,pm) =
            [T.real_pp r_prec st
            ,msg
            ,data_value_pp r_prec mnn
            ,data_value_pp r_prec vel
            ,show ch
            ,param_pp r_prec pm]
        with_hdr dat = (Just csv_mnd_hdr,dat)
    in T.csv_table_write id T.def_csv_opt nm . with_hdr . map un_node

-- * MND Seq forms

-- | (p0=midi-note,p1=velocity,channel,param)
type Event n = (n,n,Channel,[Param])

-- | Translate from 'Tseq' form to 'Wseq' form.
midi_tseq_to_midi_wseq :: (Num t,Eq n) => T.Tseq t (T.On_Off (Event n)) -> T.Wseq t (Event n)
midi_tseq_to_midi_wseq = T.tseq_on_off_to_wseq (\(n0,_,c0,_) (n1,_,c1,_) -> c0 == c1 && n0 == n1)

midi_wseq_to_midi_tseq :: (Num t,Ord t) => T.Wseq t x -> T.Tseq t (T.On_Off x)
midi_wseq_to_midi_tseq = T.wseq_on_off

-- | Ignores non on/off messages.
mnd_to_tseq :: Num n => [MND t n] -> T.Tseq t (T.On_Off (Event n))
mnd_to_tseq =
    let mk_node (st,msg,mnn,vel,ch,pm) =
            case msg of
              "on" -> Just (st,T.On (mnn,vel,ch,pm))
              "off" -> Just (st,T.Off (mnn,0,ch,pm))
              _ -> Nothing
    in mapMaybe mk_node

-- | 'Tseq' form of 'csv_mnd_read', channel information is retained, off-velocity is zero.
csv_mnd_read_tseq :: (Read t,Real t,Read n,Real n) => FilePath -> IO (T.Tseq t (T.On_Off (Event n)))
csv_mnd_read_tseq = fmap mnd_to_tseq . csv_mnd_read

-- | 'Tseq' form of 'csv_mnd_write', data is .
csv_mnd_write_tseq :: (Real t,Real n) => Int -> FilePath -> T.Tseq t (T.On_Off (Event n)) -> IO ()
csv_mnd_write_tseq r_prec nm sq =
    let f (t,e) = case e of
                    T.On (n,v,c,p) -> (t,"on",n,v,c,p)
                    T.Off (n,_,c,p) -> (t,"off",n,0,c,p)
    in csv_mnd_write r_prec nm (map f sq)

-- * MNDD (simplifies cases where overlaps on the same channel are allowed).

-- | Message should be @note@ for note data.
csv_mndd_hdr :: [String]
csv_mndd_hdr = ["time","duration","message","note","velocity","channel","param"]

-- > unwords csv_mndd_hdr == "time duration message note velocity channel param"
type MNDD t n = (t,t,String,n,n,Channel,[Param])

csv_mndd_parse :: (Read t,Real t,Read n,Real n) => T.CSV_Table String -> [MNDD t n]
csv_mndd_parse (hdr,dat) =
    let err x = error ("csv_mndd_read: " ++ x)
        f m =
            case m of
              [st,du,msg,mnn,vel,ch,pm] ->
                  (T.reads_exact_err "time" st
                  ,T.reads_exact_err "duration" du
                  ,msg
                  ,T.reads_exact_err "note" mnn
                  ,T.reads_exact_err "velocity" vel
                  ,T.reads_exact_err "channel" ch
                  ,param_parse pm)
              _ -> err "entry?"
    in case hdr of
         Just hdr' -> if hdr' == csv_mndd_hdr then map f dat else err "header?"
         Nothing -> err "no header?"

-- | Midi note/duration data.
csv_mndd_read :: (Read t,Real t,Read n,Real n) => FilePath -> IO [MNDD t n]
csv_mndd_read = fmap csv_mndd_parse . load_csv

-- | Writer.
csv_mndd_write :: (Real t,Real n) => Int -> FilePath -> [MNDD t n] -> IO ()
csv_mndd_write r_prec nm =
    let un_node (st,du,msg,mnn,vel,ch,pm) =
            [T.real_pp r_prec st,T.real_pp r_prec du,msg
            ,data_value_pp r_prec mnn,data_value_pp r_prec vel
            ,show ch
            ,param_pp r_prec pm]
        with_hdr dat = (Just csv_mndd_hdr,dat)
    in T.csv_table_write id T.def_csv_opt nm . with_hdr . map un_node

-- * MNDD Seq forms

-- | Ignores non note messages.
mndd_to_wseq :: [MNDD t n] -> T.Wseq t (Event n)
mndd_to_wseq =
    let mk_node (st,du,msg,mnn,vel,ch,pm) =
            case msg of
              "note" -> Just ((st,du),(mnn,vel,ch,pm))
              _ -> Nothing
    in mapMaybe mk_node

-- | 'Wseq' form of 'csv_mndd_read'.
csv_mndd_read_wseq :: (Read t,Real t,Read n,Real n) => FilePath -> IO (T.Wseq t (Event n))
csv_mndd_read_wseq = fmap mndd_to_wseq . csv_mndd_read

-- | 'Wseq' form of 'csv_mndd_write'.
csv_mndd_write_wseq :: (Real t,Real n) => Int -> FilePath -> T.Wseq t (Event n) -> IO ()
csv_mndd_write_wseq r_prec nm =
    let f ((st,du),(mnn,vel,ch,pm)) = (st,du,"note",mnn,vel,ch,pm)
    in csv_mndd_write r_prec nm . map f

-- * Composite

-- | Parse either MND or MNDD data to Wseq, CSV type is decided by header.
csv_midi_parse_wseq :: (Read t,Real t,Read n,Real n) => T.CSV_Table String -> T.Wseq t (Event n)
csv_midi_parse_wseq (hdr,dat) = do
  case hdr of
    Just hdr' -> if hdr' == csv_mnd_hdr
                 then midi_tseq_to_midi_wseq (mnd_to_tseq (csv_mnd_parse (hdr,dat)))
                 else if hdr' == csv_mndd_hdr
                      then mndd_to_wseq (csv_mndd_parse (hdr,dat))
                      else error "csv_midi_read_wseq: not MND or MNDD"
    _ -> error "csv_midi_read_wseq: header?"

csv_midi_read_wseq :: (Read t,Real t,Read n,Real n) => FilePath -> IO (T.Wseq t (Event n))
csv_midi_read_wseq = fmap csv_midi_parse_wseq . load_csv
