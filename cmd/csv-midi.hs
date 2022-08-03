import System.Environment {- base -}

import Music.Theory.Array.Csv.Midi.Cli {- hmt -}

main :: IO ()
main = getArgs >>= csv_midi_cli
