-- | Functions (partial) for reading & writing SKINI data files.
--
-- <https://ccrma.stanford.edu/software/stk/skini.html>
module Music.Theory.Array.CSV.Midi.SKINI where

import Data.List {- base -}

import qualified Music.Theory.Array.CSV.Midi.MND as T {- hmt -}
import qualified Music.Theory.Time.Seq as T {- hmt -}

-- | SKINI allows delta or absolute time-stamps.
data TIME t = Delta t | Absolute t

-- | SKINI data type of (message,time-stamp,channel,data-one,data-two)
type SKINI t n = (String,TIME t,T.Channel,n,n)

mnd_msg_to_skini_msg :: String -> String
mnd_msg_to_skini_msg msg =
  case msg of
    "on" -> "NoteOn"
    "off" -> "NoteOff"
    _ -> error "mnd_msg_to_skini_msg"

mnd_to_skini_f :: (t -> TIME t) -> T.MND t n -> SKINI t n
mnd_to_skini_f f mnd =
  case mnd of
    (t,msg,d1,d2,ch,[]) -> (mnd_msg_to_skini_msg msg,f t,ch,d1,d2)
    _ -> error "mnd_to_skini"

mnd_to_skini_abs :: T.MND t n -> SKINI t n
mnd_to_skini_abs = mnd_to_skini_f Absolute

midi_tseq_to_skini_seq :: (Num t,Eq n) => T.Tseq t (T.Begin_End (T.Event n)) -> [SKINI t n]
midi_tseq_to_skini_seq =
  let f e =
        case e of
          (t,T.Begin (d1,d2,ch,[])) -> ("NoteOn",Delta t,ch,d1,d2)
          (t,T.End (d1,d2,ch,[])) -> ("NoteOff",Delta t,ch,d1,d2)
          _ -> error "midi_tseq_to_skini_seq"
  in map f . T.tseq_to_iseq

time_pp :: Real t => Int -> TIME t -> String
time_pp k t =
  case t of
    Delta x -> T.data_value_pp k x
    Absolute x -> '=' : T.data_value_pp k x

skini_pp_csv :: (Real t,Real n) => Int -> SKINI t n -> String
skini_pp_csv k (msg,t,ch,d1,d2) =
  let f = T.data_value_pp k
  in intercalate "," [msg,time_pp k t,show ch,f d1,f d2]

-- > let fn = "/home/rohan/sw/hmt/data/csv/mnd/1080-C01.csv"
-- > m <- T.csv_mnd_read_tseq fn :: IO (T.Tseq Double (T.Begin_End (T.Event Int)))
-- > skini_write_csv 4 "/tmp/t.skini" (midi_tseq_to_skini_seq m)
skini_write_csv :: (Real t,Real n) => Int -> FilePath -> [SKINI t n] -> IO ()
skini_write_csv k fn = writeFile fn . unlines . map (skini_pp_csv k)
