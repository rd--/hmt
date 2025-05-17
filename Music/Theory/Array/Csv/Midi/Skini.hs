{- | Functions (partial) for reading & writing Skini data files.

<https://ccrma.stanford.edu/software/stk/skini.html>
-}
module Music.Theory.Array.Csv.Midi.Skini where

import Data.List {- base -}

import qualified Music.Theory.Array.Csv.Midi.Mnd as Mnd {- hmt -}
import qualified Music.Theory.Time.Seq as Seq {- hmt -}

-- | Skini allows delta or absolute time-stamps.
data Time t = Delta t | Absolute t

-- | Skini data type of (message,time-stamp,channel,data-one,data-two)
type Skini t n = (String, Time t, Mnd.Channel, n, n)

mnd_msg_to_skini_msg :: String -> String
mnd_msg_to_skini_msg msg =
  case msg of
    "on" -> "NoteOn"
    "off" -> "NoteOff"
    _ -> error "mnd_msg_to_skini_msg"

mnd_to_skini_f :: (t -> Time t) -> Mnd.Mnd t n -> Skini t n
mnd_to_skini_f f mnd =
  case mnd of
    (t, msg, d1, d2, ch, []) -> (mnd_msg_to_skini_msg msg, f t, ch, d1, d2)
    _ -> error "mnd_to_skini"

mnd_to_skini_abs :: Mnd.Mnd t n -> Skini t n
mnd_to_skini_abs = mnd_to_skini_f Absolute

midi_tseq_to_skini_seq :: (Num t, Eq n) => Seq.Tseq t (Seq.Begin_End (Mnd.Event n)) -> [Skini t n]
midi_tseq_to_skini_seq =
  let f e =
        case e of
          (t, Seq.Begin (d1, d2, ch, [])) -> ("NoteOn", Delta t, ch, d1, d2)
          (t, Seq.End (d1, d2, ch, [])) -> ("NoteOff", Delta t, ch, d1, d2)
          _ -> error "midi_tseq_to_skini_seq"
  in map f . Seq.tseq_to_iseq

time_pp :: Real t => Int -> Time t -> String
time_pp k t =
  case t of
    Delta x -> Mnd.data_value_pp k x
    Absolute x -> '=' : Mnd.data_value_pp k x

skini_pp_csv :: (Real t, Real n) => Int -> Skini t n -> String
skini_pp_csv k (msg, t, ch, d1, d2) =
  let f = Mnd.data_value_pp k
  in intercalate "," [msg, time_pp k t, show ch, f d1, f d2]

{- | Write Skini

>>> let fileName = "/home/rohan/sw/hmt/data/csv/mnd/1080-C01.csv"
>>> mnd <- Mnd.csv_mnd_read_tseq fileName :: IO (Seq.Tseq Double (Seq.Begin_End (Mnd.Event Int)))
>>> length mnd
1800

> skini_write_csv 4 "/tmp/t.skini" (midi_tseq_to_skini_seq mnd)
-}
skini_write_csv :: (Real t, Real n) => Int -> FilePath -> [Skini t n] -> IO ()
skini_write_csv k fn = writeFile fn . unlines . map (skini_pp_csv k)
