import System.Environment {- base -}

import qualified Music.Theory.Array.CSV.Midi.MND as T
import qualified Music.Theory.Time.Seq as T {- hmt -}

usage :: [String]
usage = ["csv-mnd-to-csv-mndd {i|r} precision:int csv-mnd-file csv-mndd-file"]

read_wseq_i :: FilePath -> IO (T.Wseq Double (Int,Int,T.Channel,[T.Param]))
read_wseq_i = T.csv_midi_read_wseq

-- > csv_mnd_onsets 4 "/home/rohan/uc/invisible/heliotrope/csv/rough/00.csv" "/dev/stdout"
csv_mnd_onsets :: Int -> FilePath -> FilePath -> IO ()
csv_mnd_onsets p i_fn o_fn = do
  m <- read_wseq_i i_fn

-- > csv_mnd_to_csv_mndd 4 "/home/rohan/uc/invisible/heliotrope/csv/rough/00.csv" "/tmp/t.csv"
csv_mnd_to_csv_mndd :: Int -> FilePath -> FilePath -> IO ()
csv_mnd_to_csv_mndd p i_fn o_fn = do
  m <- read_wseq_i i_fn
  T.csv_mndd_write_wseq p o_fn m

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["csv-mnd-to-csv-mndd","i",p,i_fn,o_fn] -> csv_mnd_to_csv_mndd (read p) i_fn o_fn
    _ -> putStrLn (unlines usage)
