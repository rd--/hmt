-- | Functions for reading midi note data from CSV files.
module Music.Theory.Array.CSV.Midi where

import Data.Function {- base -}

import qualified Music.Theory.Array.CSV as T {- hmt -}
import qualified Music.Theory.Time.Seq as T {- hmt -}

csv_midi_note_data_hdr :: [String]
csv_midi_note_data_hdr = ["time","on/off","note","velocity"]

-- | Midi note data, header is @time,on/off,note,velocity@.
-- Requires on/off translation values.
csv_midi_note_data_read :: (Read t,Real t,Read n,Real n) => (m,m) -> FilePath -> IO [(t,m,n,n)]
csv_midi_note_data_read (m_on,m_off) =
    let err x = error ("csv_midi_note_data_read: " ++ x)
        read_md x = case x of
                      "on" -> m_on
                      "off" -> m_off
                      _ -> err "on/off?"
        f m =
            case m of
              [st,md,mnn,amp] -> (read st,read_md md,read mnn,read amp)
              _ -> err "entry?"
        g (hdr,dat) = case hdr of
                        Just hdr' -> if hdr' == csv_midi_note_data_hdr then dat else err "header?"
                        Nothing -> err "no header?"
    in fmap (map f . g) . T.csv_table_read (True,',',False) id

-- | 'Tseq' form of 'csv_read_midi_note_data'.
midi_tseq_read :: (Read t,Real t,Read n,Real n) => FilePath -> IO (T.Tseq t (T.On_Off (n,n)))
midi_tseq_read =
    let mk_node (st,md,mnn,amp) = if md
                                  then (st,T.On (mnn,amp))
                                  else (st,T.Off (mnn,0))
    in fmap (map mk_node) . csv_midi_note_data_read (True,False)

-- | Translate from 'Tseq' form to 'Wseq' form.
midi_tseq_to_midi_wseq :: (Num t,Eq n) => T.Tseq t (T.On_Off (n,n)) -> T.Wseq t (n,n)
midi_tseq_to_midi_wseq = T.tseq_on_off_to_wseq ((==) `on` fst)

-- | Writer.
csv_midi_note_data_write :: (Eq m,Show t,Real t,Show n,Real n) => (m,m) -> FilePath -> [(t,m,n,n)] -> IO ()
csv_midi_note_data_write (m_on,m_off) nm =
    let show_md md = if md == m_on
                     then "on" else if md == m_off
                                    then "off"
                                    else error "csv_midi_note_data_write"
        un_node (st,md,mnn,amp) = [show st,show_md md,show mnn,show amp]
        with_hdr dat = (Just csv_midi_note_data_hdr,dat)
    in T.csv_table_write id nm . with_hdr . map un_node
