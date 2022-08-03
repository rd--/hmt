import System.Environment {- base -}

import Music.Theory.Db.Cli {- hmt -}

main :: IO ()
main = getArgs >>= db_cli
