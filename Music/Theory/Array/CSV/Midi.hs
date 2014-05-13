-- | Functions for reading midi note data from CSV files.
module Music.Theory.Array.CSV.Midi where

import Data.Function {- base -}

import qualified Music.Theory.Array.CSV as T {- hmt -}
import qualified Music.Theory.Time.Seq as T {- hmt -}

-- | Midi note data, header is @time,on/off,note,velocity@.
-- Requires on/off translation values.
csv_read_midi_note_data :: (Read t,Real t,Read n,Real n) => (m,m) -> FilePath -> IO [(t,m,n,n)]
csv_read_midi_note_data (m_on,m_off) =
    let read_md x = case x of
                      "on" -> m_on
                      "off" -> m_off
                      _ -> error "csv_read_midi_note_data: on/off?"
        f m =
            case m of
              [st,md,mnn,amp] -> (read st,read_md md,read mnn,read amp)
              _ -> error "csv_read_midi_note_data: entry?"
        g (hdr,dat) = case hdr of
                        Just ["time","on/off","note","velocity"] -> dat
                        _ -> error "csv_read_midi_note_data: header?"
    in fmap (map f . g) . T.csv_table_read (True,',',False) id

-- | 'Tseq' form of 'csv_read_midi_note_data'.
midi_tseq_read :: (Read t,Real t,Read n,Real n) => FilePath -> IO (T.Tseq t (T.On_Off (n,n)))
midi_tseq_read =
    let mk_node (st,md,mnn,amp) = if md
                                  then (st,T.On (mnn,amp))
                                  else (st,T.Off (mnn,0))
    in fmap (map mk_node) . csv_read_midi_note_data (True,False)

-- | Translate from 'Tseq' form to 'Wseq' form.
midi_tseq_to_midi_wseq :: (Num t,Eq n) => T.Tseq t (T.On_Off (n,n)) -> T.Wseq t (n,n)
midi_tseq_to_midi_wseq = T.tseq_on_off_to_wseq ((==) `on` fst)
