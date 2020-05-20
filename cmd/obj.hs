import System.Environment {- base -}

import qualified Music.Theory.Graph.OBJ as T {- hmt -}

cli_obj_to_v3_graph :: FilePath -> IO ()
cli_obj_to_v3_graph obj_fn = do
  g <- T.obj_load_v3_graph_f64 obj_fn
  putStrLn (show g)

cli_v3_graph_to_obj :: Int -> FilePath -> IO ()
cli_v3_graph_to_obj prec hs_fn = do
  txt <- readFile hs_fn
  let g = read txt
  putStrLn (unlines (T.v3_graph_to_obj_f64 prec g))

cli_obj_help :: [String]
cli_obj_help =
  ["obj obj-to-v3-graph obj-file"
  ,"obj v3-graph-to-obj precision:int hs-file"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["obj-to-v3-graph",obj_fn] -> cli_obj_to_v3_graph obj_fn
    ["v3-graph-to-obj",prec,hs_fn] -> cli_v3_graph_to_obj (read prec) hs_fn
    _ -> putStrLn (unlines cli_obj_help)
