import System.Environment {- base -}

import qualified Sound.Sc3.Data.Geometry.Obj as Obj {- hsc3-data -}

cli_obj_to_v3_graph :: FilePath -> IO ()
cli_obj_to_v3_graph obj_fn = do
  g <- Obj.obj_load_lbl_ obj_fn
  putStrLn (show g)

cli_v3_graph_to_obj :: Int -> FilePath -> IO ()
cli_v3_graph_to_obj prec hs_fn = do
  txt <- readFile hs_fn
  let g = read txt
  Obj.obj_store_lbl_ prec "/dev/stdout" g

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
