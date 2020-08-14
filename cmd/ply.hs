import System.Environment {- base -}

import qualified Data.ByteString {- bytestring -}
import qualified Data.Vector {- vector -}

import qualified PLY {- ply-loader -}
import qualified PLY.Types {- ply-loader -}

import qualified Sound.SC3.Data.Geometry.PLY as PLY {- hsc3-data -}

import qualified Music.Theory.Graph.Type as T {- hmt -}

ply_load_v3_e2 :: FilePath -> IO ([(Double,Double,Double)],[(Int,Int)])
ply_load_v3_e2 fn = do
  let err = either (error "ply_to_v3_graph?") id
      v2 l = case l of {[i,j] -> (i,j);_ -> error "v2?"}
      v3 l = case l of {[i,j,k] -> (i,j,k);_ -> error "v3?"}
      flt n = case n of {PLY.Types.Sfloat r -> realToFrac r;_ -> error "flt?"}
      int n = case n of {PLY.Types.Sint r -> r;_ -> error "int?"}
  d <- fmap err (PLY.loadHeader fn)
  let str = Data.ByteString.pack . map (fromIntegral . fromEnum)
      lst = Data.Vector.toList
      v = err (PLY.loadPlyElements (str "vertex") d)
      e = err (PLY.loadPlyElements (str "edge") d)
      v_f = v3 . map flt . lst
      e_f = v2 . map int . lst
  return (map v_f (lst v),map e_f (lst e))

ply_to_v3_graph :: FilePath -> IO (T.LBL (Double,Double,Double) ())
ply_to_v3_graph fn = do
  (v,e) <- ply_load_v3_e2 fn
  return (zip [0..] v,zip e (repeat ()))

cli_ply_to_v3_graph :: FilePath -> IO ()
cli_ply_to_v3_graph ply_fn = do
  g <- ply_to_v3_graph ply_fn
  putStrLn (show g)

cli_v3_graph_to_ply :: Int -> FilePath -> IO ()
cli_v3_graph_to_ply prec hs_fn = do
  txt <- readFile hs_fn
  let g = read txt
  putStrLn (unlines (PLY.v3_graph_to_ply_clr prec g))

cli_ply_help :: [String]
cli_ply_help =
  ["ply ply-to-v3-graph ply-file"
  ,"ply v3-graph-to-ply precision:int hs-file"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["ply-to-v3-graph",ply_fn] -> cli_ply_to_v3_graph ply_fn
    ["v3-graph-to-ply",prec,hs_fn] -> cli_v3_graph_to_ply (read prec) hs_fn
    _ -> putStrLn (unlines cli_ply_help)
