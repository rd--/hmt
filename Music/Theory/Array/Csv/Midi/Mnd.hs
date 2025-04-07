{- | Functions for reading midi note data (Mnd) from Csv files.

This is /not/ a generic text midi notation.
The required columns are documented at `Mnd` and `Mndd`.
The defined commands are @on@ and @off@, but others may be present.
Non-integral note number and key velocity data are allowed.
-}
module Music.Theory.Array.Csv.Midi.Mnd where

import Data.Function {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import Data.List.Split {- split -}

import qualified Music.Theory.Array.Csv as Csv {- hmt-base -}
import qualified Music.Theory.Math as Math {- hmt-base -}
import qualified Music.Theory.Read as Read {- hmt-base -}
import qualified Music.Theory.Show as Show {- hmt-base -}

import qualified Music.Theory.Time.Seq as Seq {- hmt -}

-- * Param ; Sound.Sc3.Server.Param

type Param = [(String, Double)]

param_parse :: (Char, Char) -> String -> Param
param_parse (c1, c2) str =
  let f x = case splitOn [c2] x of
        [lhs, rhs] -> (lhs, read rhs)
        _ -> error ("param_parse: " ++ x)
  in if null str then [] else map f (splitOn [c1] str)

param_pp :: (Char, Char) -> Int -> Param -> String
param_pp (c1, c2) k =
  let f (lhs, rhs) = concat [lhs, [c2], Show.double_pp k rhs]
  in intercalate [c1] . map f

-- * Mnd

-- | If /r/ is whole to /k/ places then show as integer, else as float to /k/ places.
data_value_pp :: Real t => Int -> t -> String
data_value_pp k r =
  if Math.whole_to_precision k r
    then show (Math.real_floor_int r)
    else Show.real_pp k r

-- | Channel values are 4-bit (0-15).
type Channel = Int

-- | The required header (column names) field.
csv_mnd_hdr :: [String]
csv_mnd_hdr = ["time", "on/off", "note", "velocity", "channel", "param"]

{- | Midi note data, the type parameters are to allow for fractional note & velocity values.

The command is a string, @on@ and @off@ are standard, other commands may be present.
note and velocity data is (0-127), channel is (0-15), param are ;-separated key:string=value:float.

>>> unwords csv_mnd_hdr
"time on/off note velocity channel param"

> let all_notes_off = zipWith (\t k -> (t,"off",k,0,0,[])) [0.0,0.01 ..] [0 .. 127]
> csv_mnd_write 4 "/home/rohan/sw/hmt/data/csv/mnd/all-notes-off.csv" all_notes_off
-}
type Mnd t n = (t, String, n, n, Channel, Param)

csv_mnd_parse_f :: (Read t, Real t, Read n, Real n) => (n -> m) -> Csv.Csv_Table String -> [Mnd t m]
csv_mnd_parse_f cnv (hdr, dat) =
  let err x = error ("csv_mnd_read: " ++ x)
      f m = case m of
        [st, msg, mnn, vel, ch, pm] ->
          ( Read.reads_exact_err "time:real" st
          , msg
          , cnv (Read.reads_exact_err "note:real" mnn)
          , cnv (Read.reads_exact_err "velocity:real" vel)
          , Read.reads_exact_err "channel:int" ch
          , param_parse (';', '=') pm
          )
        _ -> err "entry?"
  in case hdr of
      Just hdr' -> if hdr' == csv_mnd_hdr then map f dat else err "header?"
      Nothing -> err "no header?"

csv_mnd_parse :: (Read t, Real t, Read n, Real n) => Csv.Csv_Table String -> [Mnd t n]
csv_mnd_parse = csv_mnd_parse_f id

load_csv :: FilePath -> IO (Csv.Csv_Table String)
load_csv = Csv.csv_table_read (True, ',', False, Csv.Csv_No_Align) id

{- | Midi note data.

>>> let fileName = "/home/rohan/sw/hmt/data/csv/mnd/1080-C01.csv"
>>> mnd <- csv_mnd_read fileName :: IO [Mnd Double Int]
>>> length mnd
1800

> csv_mnd_write 4 "/tmp/t.csv" mnd
-}
csv_mnd_read :: (Read t, Real t, Read n, Real n) => FilePath -> IO [Mnd t n]
csv_mnd_read = fmap csv_mnd_parse . load_csv

-- | Writer.
csv_mnd_write :: (Real t, Real n) => Int -> FilePath -> [Mnd t n] -> IO ()
csv_mnd_write r_prec nm =
  let un_node (st, msg, mnn, vel, ch, pm) =
        [ Show.real_pp r_prec st
        , msg
        , data_value_pp r_prec mnn
        , data_value_pp r_prec vel
        , show ch
        , param_pp (';', '=') r_prec pm
        ]
      with_hdr dat = (Just csv_mnd_hdr, dat)
  in Csv.csv_table_write id Csv.def_csv_opt nm . with_hdr . map un_node

-- * Mnd Seq forms

-- | (p0=midi-note,p1=velocity,channel,param)
type Event n = (n, n, Channel, Param)

-- | mnn = midi-note-number
event_mnn :: Event t -> t
event_mnn (mnn, _, _, _) = mnn

-- | ch = channel
event_ch :: Event t -> Channel
event_ch (_, _, ch, _) = ch

-- | Are events equal at mnn field?
event_eq_mnn :: Eq t => Event t -> Event t -> Bool
event_eq_mnn = (==) `on` event_mnn

-- | Are events equal at mnn and ch fields?
event_eq_ol :: Eq t => Event t -> Event t -> Bool
event_eq_ol = (==) `on` (\(mnn, _, ch, _) -> (mnn, ch))

-- | Apply (mnn-f,vel-f,ch-f,param-f) to Event.
event_map :: (t -> u, t -> u, Channel -> Channel, Param -> Param) -> Event t -> Event u
event_map (f1, f2, f3, f4) (mnn, vel, ch, param) = (f1 mnn, f2 vel, f3 ch, f4 param)

-- | Apply /f/ at mnn and vel fields.
event_cast :: (t -> u) -> Event t -> Event u
event_cast f = event_map (f, f, id, id)

-- | Add /x/ to mnn field.
event_transpose :: Num a => a -> Event a -> Event a
event_transpose x = event_map ((+) x, id, id, id)

-- | Translate from 'Tseq' form to 'Wseq' form.
midi_tseq_to_midi_wseq :: (Num t, Eq n) => Seq.Tseq t (Seq.Begin_End (Event n)) -> Seq.Wseq t (Event n)
midi_tseq_to_midi_wseq = Seq.tseq_begin_end_to_wseq (\(n0, _, c0, _) (n1, _, c1, _) -> c0 == c1 && n0 == n1)

midi_wseq_to_midi_tseq :: (Num t, Ord t) => Seq.Wseq t x -> Seq.Tseq t (Seq.Begin_End x)
midi_wseq_to_midi_tseq = Seq.wseq_begin_end

-- | Ignores non on/off messages.
mnd_to_tseq :: Num n => [Mnd t n] -> Seq.Tseq t (Seq.Begin_End (Event n))
mnd_to_tseq =
  let mk_node (st, msg, mnn, vel, ch, pm) =
        case msg of
          "on" -> Just (st, Seq.Begin (mnn, vel, ch, pm))
          "off" -> Just (st, Seq.End (mnn, 0, ch, pm))
          _ -> Nothing
  in mapMaybe mk_node

-- | 'Tseq' form of 'csv_mnd_read', channel information is retained, off-velocity is zero.
csv_mnd_read_tseq :: (Read t, Real t, Read n, Real n) => FilePath -> IO (Seq.Tseq t (Seq.Begin_End (Event n)))
csv_mnd_read_tseq = fmap mnd_to_tseq . csv_mnd_read

-- | 'Tseq' form of 'csv_mnd_write', data is .
csv_mnd_write_tseq :: (Real t, Real n) => Int -> FilePath -> Seq.Tseq t (Seq.Begin_End (Event n)) -> IO ()
csv_mnd_write_tseq r_prec nm sq =
  let f (t, e) = case e of
        Seq.Begin (n, v, c, p) -> (t, "on", n, v, c, p)
        Seq.End (n, _, c, p) -> (t, "off", n, 0, c, p)
  in csv_mnd_write r_prec nm (map f sq)

-- * Mndd (simplifies cases where overlaps on the same channel are allowed).

-- | Message should be @note@ for note data.
csv_mndd_hdr :: [String]
csv_mndd_hdr = ["time", "duration", "message", "note", "velocity", "channel", "param"]

{- | Midi note/duration data.
The type parameters are to allow for fractional note & velocity values.
The command is a string, @note@ is standard, other commands may be present.

>>> unwords csv_mndd_hdr
"time duration message note velocity channel param"
-}
type Mndd t n = (t, t, String, n, n, Channel, Param)

-- | Compare sequence is: start-time,channel-number,note-number,velocity,duration,param.
mndd_compare :: (Ord t, Ord n) => Mndd t n -> Mndd t n -> Ordering
mndd_compare x1 x2 =
  case (x1, x2) of
    ((t1, d1, "note", n1, v1, c1, p1), (t2, d2, "note", n2, v2, c2, p2)) ->
      compare (t1, c1, n1, v1, d1, p1) (t2, c2, n2, v2, d2, p2)
    _ -> compare x1 x2

csv_mndd_parse_f :: (Read t, Real t, Read n, Real n) => (n -> m) -> Csv.Csv_Table String -> [Mndd t m]
csv_mndd_parse_f cnv (hdr, dat) =
  let err x = error ("csv_mndd_read: " ++ x)
      f m =
        case m of
          [st, du, msg, mnn, vel, ch, pm] ->
            ( Read.reads_exact_err "time:real" st
            , Read.reads_exact_err "duration:real" du
            , msg
            , cnv (Read.reads_exact_err "note:real" mnn)
            , cnv (Read.reads_exact_err "velocity:real" vel)
            , Read.reads_exact_err "channel:int" ch
            , param_parse (';', '=') pm
            )
          _ -> err "entry?"
  in case hdr of
      Just hdr' -> if hdr' == csv_mndd_hdr then map f dat else err "header?"
      Nothing -> err "no header?"

-- | Pars midi note/duration data from Csv table.
csv_mndd_parse :: (Read t, Real t, Read n, Real n) => Csv.Csv_Table String -> [Mndd t n]
csv_mndd_parse = csv_mndd_parse_f id

-- | 'csv_mndd_parse' of 'load_csv'
csv_mndd_read :: (Read t, Real t, Read n, Real n) => FilePath -> IO [Mndd t n]
csv_mndd_read = fmap csv_mndd_parse . load_csv

-- | Writer.
csv_mndd_write :: (Real t, Real n) => Int -> FilePath -> [Mndd t n] -> IO ()
csv_mndd_write r_prec nm =
  let un_node (st, du, msg, mnn, vel, ch, pm) =
        [ Show.real_pp r_prec st
        , Show.real_pp r_prec du
        , msg
        , data_value_pp r_prec mnn
        , data_value_pp r_prec vel
        , show ch
        , param_pp (';', '=') r_prec pm
        ]
      with_hdr dat = (Just csv_mndd_hdr, dat)
  in Csv.csv_table_write id Csv.def_csv_opt nm . with_hdr . map un_node

-- * Mndd Seq forms

-- | Ignores non note messages.
mndd_to_wseq :: [Mndd t n] -> Seq.Wseq t (Event n)
mndd_to_wseq =
  let mk_node (st, du, msg, mnn, vel, ch, pm) =
        case msg of
          "note" -> Just ((st, du), (mnn, vel, ch, pm))
          _ -> Nothing
  in mapMaybe mk_node

-- | 'Wseq' form of 'csv_mndd_read'.
csv_mndd_read_wseq :: (Read t, Real t, Read n, Real n) => FilePath -> IO (Seq.Wseq t (Event n))
csv_mndd_read_wseq = fmap mndd_to_wseq . csv_mndd_read

-- | 'Wseq' form of 'csv_mndd_write'.
csv_mndd_write_wseq :: (Real t, Real n) => Int -> FilePath -> Seq.Wseq t (Event n) -> IO ()
csv_mndd_write_wseq r_prec nm =
  let f ((st, du), (mnn, vel, ch, pm)) = (st, du, "note", mnn, vel, ch, pm)
  in csv_mndd_write r_prec nm . map f

-- * Composite

-- | Parse either Mnd or Mndd data to Wseq, Csv type is decided by header.
csv_midi_parse_wseq_f :: (Read t, Real t, Read n, Real n, Num m, Eq m) => (n -> m) -> Csv.Csv_Table String -> Seq.Wseq t (Event m)
csv_midi_parse_wseq_f cnv (hdr, dat) = do
  case hdr of
    Just hdr' ->
      if hdr' == csv_mnd_hdr
        then midi_tseq_to_midi_wseq (mnd_to_tseq (csv_mnd_parse_f cnv (hdr, dat)))
        else
          if hdr' == csv_mndd_hdr
            then mndd_to_wseq (csv_mndd_parse_f cnv (hdr, dat))
            else error "csv_midi_read_wseq: not Mnd or Mndd"
    _ -> error "csv_midi_read_wseq: header?"

csv_midi_parse_wseq :: (Read t, Real t, Read n, Real n) => Csv.Csv_Table String -> Seq.Wseq t (Event n)
csv_midi_parse_wseq = csv_midi_parse_wseq_f id

csv_midi_read_wseq :: (Read t, Real t, Read n, Real n) => FilePath -> IO (Seq.Wseq t (Event n))
csv_midi_read_wseq = fmap csv_midi_parse_wseq . load_csv
