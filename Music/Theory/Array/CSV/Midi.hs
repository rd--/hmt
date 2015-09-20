-- | Functions for reading midi note data from CSV files.
-- This is /not/ a generic text midi notation.
-- The defined commands are @on@ and @off@, but others may be present.
module Music.Theory.Array.CSV.Midi where

import Data.Function {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.Array.CSV as T {- hmt -}
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
reads_err str = fromMaybe (error ("could not read: " ++ str)) (reads_exact str)

-- | The required header field, header is @time,on/off,note,velocity,channel@.
csv_midi_hdr :: [String]
csv_midi_hdr = ["time","on/off","note","velocity","channel"]

-- | Midi note data.  Translation values for on/off are consulted.
--
-- > let fn = "/home/rohan/cvs/uc/uc-26/daily-practice/2014-08-13.1.csv"
-- > m <- csv_midi_read fn :: IO [(Double,Bool,Double,Double,Int)]
-- > length m == 17655
csv_midi_read :: (Read t,Real t,Read n,Real n) => FilePath -> IO [(t,String,n,n,Int)]
csv_midi_read =
    let err x = error ("csv_midi_read: " ++ x)
        f m =
            case m of
              [st,md,mnn,amp,ch] -> (reads_err st
                                    ,md
                                    ,reads_err mnn
                                    ,reads_err amp
                                    ,reads_err ch)
              _ -> err "entry?"
        g (hdr,dat) = case hdr of
                        Just hdr' -> if hdr' == csv_midi_hdr then dat else err "header?"
                        Nothing -> err "no header?"
    in fmap (map f . g) . T.csv_table_read (True,',',False,T.CSV_No_Align) id

-- | Writer.
csv_midi_write :: (Show t,Real t,Show n,Real n) => FilePath -> [(t,String,n,n,Int)] -> IO ()
csv_midi_write nm =
    let un_node (st,md,mnn,amp,ch) = [show st,md,show mnn,show amp,show ch]
        with_hdr dat = (Just csv_midi_hdr,dat)
    in T.csv_table_write id T.def_csv_opt nm . with_hdr . map un_node

-- * Seq forms

-- | 'Tseq' form of 'csv_midi_read', channel information is discarded.
midi_tseq_read :: (Read t,Real t,Read n,Real n) => FilePath -> IO (T.Tseq t (T.On_Off (n,n)))
midi_tseq_read =
    let mk_node (st,md,mnn,amp,_) = case md of
                                      "on" -> Just (st,T.On (mnn,amp))
                                      "off" -> Just (st,T.Off (mnn,0))
                                      _ -> Nothing
    in fmap (mapMaybe mk_node) . csv_midi_read

-- | Translate from 'Tseq' form to 'Wseq' form.
midi_tseq_to_midi_wseq :: (Num t,Eq n) => T.Tseq t (T.On_Off (n,n)) -> T.Wseq t (n,n)
midi_tseq_to_midi_wseq = T.tseq_on_off_to_wseq ((==) `on` fst)

-- | Off-velocity is zero.
midi_wseq_to_midi_tseq :: (Num t,Ord t) => T.Wseq t x -> T.Tseq t (T.On_Off x)
midi_wseq_to_midi_tseq = T.wseq_on_off

-- | 'Tseq' form of 'csv_midi_write', data is (midi-note,velocity,channel).
midi_tseq_write :: (Show t,Real t,Show n,Real n) => FilePath -> T.Tseq t (T.On_Off (n,n,Int)) -> IO ()
midi_tseq_write nm sq =
    let f (t,e) = case e of
                    T.On (n,v,c) -> (t,"on",n,v,c)
                    T.Off (n,v,c) -> (t,"off",n,v,c)
        sq' = map f sq
    in csv_midi_write nm sq'
