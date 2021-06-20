import System.Environment {- base -}

import qualified Music.Theory.Array.CSV.Midi.MND as T {- hmt -}
import qualified Music.Theory.Time.Seq as T {- hmt -}

usage :: [String]
usage =
  ["concat {r} -o output-file input-file..."
  ,"mnd-to-mndd {i|r} precision:int input-file output-file"
  ,"mndd-transpose precision:int n:int input-file output-file"]

read_wseq_i :: FilePath -> IO (T.Wseq Double (T.Event Int))
read_wseq_i = T.csv_midi_read_wseq

read_wseq_r :: FilePath -> IO (T.Wseq Double (T.Event Double))
read_wseq_r = T.csv_midi_read_wseq

mnd_to_mndd_i :: Int -> FilePath -> FilePath -> IO ()
mnd_to_mndd_i p i_fn o_fn = do
  m <- read_wseq_i i_fn
  T.csv_mndd_write_wseq p o_fn m

mndd_transpose_r :: Int -> Double -> FilePath -> FilePath -> IO ()
mndd_transpose_r p k i_fn o_fn = do
  m <- read_wseq_r i_fn
  let f (t,(mnn,vel,ch,pr)) = (t,(mnn + k,vel,ch,pr))
  T.csv_mndd_write_wseq p o_fn (map f m)

csv_midi_concat_r :: FilePath -> [FilePath] -> IO ()
csv_midi_concat_r o_fn i_fn = do
  i <- mapM read_wseq_r i_fn
  T.csv_mndd_write_wseq 4 o_fn (T.wseq_concat i)

main :: IO ()
main = do
  a <- getArgs
  case a of
    "concat":"r":"-o":o_fn:i_fn -> csv_midi_concat_r o_fn i_fn
    ["mnd-to-mndd","i",p,i_fn,o_fn] -> mnd_to_mndd_i (read p) i_fn o_fn
    ["mndd-transpose","r",p,k,i_fn,o_fn] -> mndd_transpose_r (read p) (read k) i_fn o_fn
    _ -> putStrLn (unlines usage)

{-
fn = "/home/rohan/uc/invisible/heliotrope/csv/rough/00.csv"
mnd_to_mndd_i 4 fn "/tmp/t-mndd.csv"
mndd_transpose_r 4 (-12) fn "/tmp/t-trs.csv"
-}

