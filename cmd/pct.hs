import System.Environment {- base -}

import Music.Theory.Z.Drape_1999.Cli {- hmt -}

main :: IO ()
main = getArgs >>= pct_cli
