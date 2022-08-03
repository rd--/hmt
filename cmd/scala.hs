import System.Environment {- base -}

import Music.Theory.Tuning.Scala.Cli {- hmt -}

main :: IO ()
main = getArgs >>= scala_cli
