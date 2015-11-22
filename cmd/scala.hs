import Data.Char {- base -}
import Data.List {- base -}
import System.Environment {- base -}

import qualified Music.Theory.Function as T {- hmt -}
import qualified Music.Theory.Tuning.Scala as T {- hmt -}

db_stat :: IO ()
db_stat = do
  db <- T.scl_load_db :: IO [T.Scale Integer]
  let po = filter (== Just (Right 2)) (map T.scale_octave db)
      uf = filter T.is_scale_uniform db
      r = ["# entries        : " ++ show (length db)
          ,"# perfect-octave : " ++ show (length po)
          ,"# scale-uniform  : " ++ show (length uf)]
  putStrLn (unlines r)

env :: IO ()
env = do
  dir <- T.scl_get_dir
  putStrLn ("SCALA_SCL_DIR = " ++ if null dir then "NOT SET" else dir)

-- > search True ["xenakis"]
-- > search True ["lamonte","young"]
search :: Bool -> [String] -> IO ()
search ci txt = do
  db <- T.scl_load_db :: IO [T.Scale Integer]
  let modify = if ci then map toLower else id
      txt' = map modify txt
      db' = filter (T.predicate_all (map isInfixOf txt') . modify . T.scale_description) db
  mapM_ (putStrLn . unlines . T.scale_stat) db'

stat_all :: IO ()
stat_all = do
  db <- T.scl_load_db :: IO [T.Scale Integer]
  mapM_ (putStrLn . unlines . T.scale_stat) db

-- > stat_by_name "young-lm_piano"
stat_by_name :: FilePath -> IO ()
stat_by_name nm = do
  sc <- T.scl_load nm :: IO (T.Scale Integer)
  putStrLn (unlines (T.scale_stat sc))

help :: [String]
help =
    ["db-stat"
    ,"env"
    ,"search ci|cs text:string..."
    ,"stat all"
    ,"stat scale-name:string|file-path"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["db-stat"] -> db_stat
    ["env"] -> env
    "search":ci:txt -> search (ci == "ci") txt
    ["stat","all"] -> stat_all
    ["stat",nm] -> stat_by_name nm
    _ -> putStrLn (unlines help)
