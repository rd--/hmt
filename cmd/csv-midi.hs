import System.Environment {- base -}

import qualified Music.Theory.Array.Csv.Midi.Cli {- hmt -}

main :: IO ()
main = getArgs >>= Music.Theory.Array.Csv.Midi.Cli.csv_midi_cli
