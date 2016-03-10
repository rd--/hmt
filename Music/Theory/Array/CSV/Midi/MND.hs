-- | Functions for reading midi note data (MND) from CSV files.
-- This is /not/ a generic text midi notation.
-- The defined commands are @on@ and @off@, but others may be present.
-- Non-integral note number and key velocity data are allowed.
module Music.Theory.Array.CSV.Midi.MND where

import Data.Maybe {- base -}
import Data.Word {- base -}
import Numeric {- base -}

import qualified Music.Theory.Array.CSV as T {- hmt -}
import qualified Music.Theory.Math as T {- hmt -}
import qualified Music.Theory.Math.Convert as T {- hmt -}
import qualified Music.Theory.Time.Seq as T {- hmt -}

-- | Variant of 'reads' requiring exact match.
--
-- > map reads_exact ["1.5","2,5"] == [Just 1.5,Nothing]
reads_exact :: Read a => String -> Maybe a
reads_exact s =
    case reads s of
      [(r,"")] -> Just r
      _ -> Nothing

-- | Variant of 'reads_exact' that errors on failure.
reads_err :: Read a => String -> a
reads_err str = fromMaybe (error ("reads_err: " ++ str)) (reads_exact str)

-- | The required header field.
csv_mnd_hdr :: [String]
csv_mnd_hdr = ["time","on/off","note","velocity","channel"]

-- | Channel values are 4-bit (0-15).
type Channel = Word8

-- | Midi note data, the type parameters are to allow for fractional note & velocity values.
-- The command is a string, @on@ and @off@ are standard, other commands may be present.
--
-- > unwords csv_mnd_hdr == "time on/off note velocity channel"
type MND t n = (t,String,n,n,Channel)

-- | Midi note data.
--
-- > let fn = "/home/rohan/cvs/uc/uc-26/daily-practice/2014-08-13.1.csv"
-- > m <- csv_mnd_read fn :: IO [MND Double Double]
-- > length m == 17655
csv_mnd_read :: (Read t,Real t,Read n,Real n) => FilePath -> IO [MND t n]
csv_mnd_read =
    let err x = error ("csv_mnd_read: " ++ x)
        f m =
            case m of
              [st,md,mnn,amp,ch] -> (reads_err st
                                    ,md
                                    ,reads_err mnn
                                    ,reads_err amp
                                    ,reads_err ch)
              _ -> err "entry?"
        g (hdr,dat) = case hdr of
                        Just hdr' -> if hdr' == csv_mnd_hdr then dat else err "header?"
                        Nothing -> err "no header?"
    in fmap (map f . g) . T.csv_table_read (True,',',False,T.CSV_No_Align) id

-- | Show /r/ as float to /k/ places.
real_pp :: Real t => Int -> t -> String
real_pp k t = showFFloat (Just k) (T.real_to_double t) ""

-- | If /r/ is whole to /k/ places then show as integer, else as float to /k/ places.
data_value_pp :: Real t => Int -> t -> String
data_value_pp k r = if T.whole_to_precision k r then show (T.real_floor_int r) else real_pp k r

-- | Writer.
csv_mnd_write :: (Show t,Real t,Show n,Real n) => Int -> FilePath -> [MND t n] -> IO ()
csv_mnd_write r_prec nm =
    let un_node (st,md,mnn,amp,ch) = [real_pp r_prec st,md
                                     ,data_value_pp r_prec mnn,data_value_pp r_prec amp
                                     ,show ch]
        with_hdr dat = (Just csv_mnd_hdr,dat)
    in T.csv_table_write id T.def_csv_opt nm . with_hdr . map un_node

-- * Seq forms

-- | 'Tseq' form of 'csv_mnd_read', channel information is retained, off-velocity is zero.
midi_tseq_read :: (Read t,Real t,Read n,Real n) => FilePath -> IO (T.Tseq t (T.On_Off (n,n,Channel)))
midi_tseq_read =
    let mk_node (st,md,mnn,amp,ch) =
            case md of
              "on" -> Just (st,T.On (mnn,amp,ch))
              "off" -> Just (st,T.Off (mnn,0,ch))
              _ -> Nothing
    in fmap (mapMaybe mk_node) . csv_mnd_read

-- | Translate from 'Tseq' form to 'Wseq' form.
midi_tseq_to_midi_wseq :: (Num t,Eq n) => T.Tseq t (T.On_Off (n,n,Channel)) -> T.Wseq t (n,n,Channel)
midi_tseq_to_midi_wseq = T.tseq_on_off_to_wseq (\(n0,_,c0) (n1,_,c1) -> c0 == c1 && n0 == n1)

midi_wseq_to_midi_tseq :: (Num t,Ord t) => T.Wseq t x -> T.Tseq t (T.On_Off x)
midi_wseq_to_midi_tseq = T.wseq_on_off

-- | 'Tseq' form of 'csv_mnd_write', data is (midi-note,velocity,channel).
midi_tseq_write :: (Show t,Real t,Show n,Real n) => Int -> FilePath -> T.Tseq t (T.On_Off (n,n,Channel)) -> IO ()
midi_tseq_write r_prec nm sq =
    let f (t,e) = case e of
                    T.On (n,v,c) -> (t,"on",n,v,c)
                    T.Off (n,_,c) -> (t,"off",n,0,c)
        sq' = map f sq
    in csv_mnd_write r_prec nm sq'
