-- | <https://users.cecs.anu.edu.au/~bdm/plantri/plantri-guide.txt>
module Music.Theory.Graph.Planar where

import System.FilePath {- filepath -}
import System.Process {- process -}
import Text.Printf {- base -}

import qualified Data.ByteString as B {- bytestring -}
import qualified Data.List.Split as S {- split -}

import qualified Music.Theory.Graph.G6 as G6 {- hmt -}
import qualified Music.Theory.Graph.Type as T {- hmt -}

-- | The 15-character header text indicating a PLANAR-CODE file.
plc_header_txt :: String
plc_header_txt = ">>planar_code<<"

-- | Read PLC header
plc_header :: B.ByteString -> String
plc_header = map (toEnum . fromIntegral) . B.unpack . B.take 15

-- | Read PLC data as list of 'Int'
plc_data :: B.ByteString -> [Int]
plc_data = map fromIntegral . B.unpack . B.drop 15

-- | Calculate length of PLC data given (n-vertices,n-edges).
plc_length :: (Int,Int) -> Int
plc_length (v,e) = v + 1 + 2 * e

-- | Scan PLC data and segment after /k/ zeros.
plc_scanner :: Int -> [Int] -> ([Int],[Int])
plc_scanner =
  let f r k i = case i of
                  0:j -> if k == 1 then (reverse (0 : r),j) else f (0 : r) (k - 1) j
                  e:j -> f (e : r) k j
                  _ -> error "plc_scanner?"
  in f []

-- | (n-vertices,clockwise-edge-sequences)
type PLC = (Int,[[Int]])

plc_n_vertices :: PLC -> Int
plc_n_vertices (k,_) = k

-- | Group PLC data into PLC structure.
plc_group :: Int -> [Int] -> PLC
plc_group k i =
  let c = S.endBy [0] i
  in if length c == k then (k,c) else error "plc_group?"

-- | Segment input data into sequence of PLC.
plc_segment :: [Int] -> [PLC]
plc_segment i =
  case i of
    [] -> []
    k:j -> case plc_scanner k j of
             (r,[]) -> [plc_group k r]
             (r,l) -> plc_group k r : plc_segment l

plc_parse :: B.ByteString -> [PLC]
plc_parse b =
  if plc_header b == plc_header_txt
  then plc_segment (plc_data b)
  else error "plc_load?"

-- | Load sequence of PLC from binary PLANAR-CODE file.
plc_load :: FilePath -> IO [PLC]
plc_load = fmap plc_parse . B.readFile

-- | All edges (one-indexed) at PLC
plc_edge_set :: PLC -> [(Int,Int)]
plc_edge_set (k,n) =
  let v = [1 .. k]
      f (i,j) = map (\x -> (i,x)) j
  in concatMap f (zip v n)

-- | Element in /x/ after /i/, the element after the last is the first.
--
-- > map (plc_next_elem "abcd") "abcd" == "bcda"
plc_next_elem :: Eq t => [t] -> t -> t
plc_next_elem x i =
  case dropWhile (/= i) x of
    [] -> error "plc_next_elem?"
    [_] -> head x
    _:j:_ -> j

-- | The next edge in PLC following /e/.
plc_next_edge :: PLC -> (Int,Int) -> (Int,Int)
plc_next_edge (_,e) (i,j) = let k = plc_next_elem (e !! (j - 1)) i in (j,k)

-- | The face of PLC starting at /e/ (one-indexed edges).
plc_face_from :: PLC -> (Int,Int) -> [(Int,Int)]
plc_face_from p e = e : takeWhile (/= e) (tail (iterate (plc_next_edge p) e))

-- | The set of all faces at PLC (one-indexed edges).
plc_face_set :: PLC -> [[(Int,Int)]]
plc_face_set p =
  let f r e =
        case e of
          [] -> reverse r
          e0:eN -> if any (e0 `elem`) r
                   then f r eN
                   else f (plc_face_from p e0 : r) eN
  in f [] (plc_edge_set p)

-- | Translate 'PLC' into un-directed 'T.G'.  PLC is one-indexed, G is zero-indexed.
plc_to_g :: PLC -> T.G
plc_to_g p =
  let (k,_) = p
      v = [0 .. k - 1]
      f (i,j) = (i - 1,j - 1)
      g (i,j) = i <= j
  in (v,filter g (map f (plc_edge_set p)))

plc_stat :: FilePath -> IO (Int, [(Int, Int, Int)])
plc_stat plc_fn = do
  p_seq <- plc_load plc_fn
  let f p = (plc_n_vertices p,length (plc_edge_set p) `div` 2,length (plc_face_set p))
  return (length p_seq,map f p_seq)

plc_stat_txt :: FilePath -> (Int, [(Int, Int, Int)]) -> [String]
plc_stat_txt fn (k,g) =
  let hdr = printf "%s G=%d" (takeBaseName fn) k
      gr ix (v,e,f) = printf " %d: V=%d E=%d F=%d" ix v e f
  in hdr : zipWith gr [1::Int ..] g

-- | Run "nauty-planarg" to convert (if possible) a set of G6 graphs to PLANAR-CODE.
g6_planarg :: [String] -> IO B.ByteString
g6_planarg =
  -- else see process-extras:readProcessWithExitCode
  let str_to_b :: String -> B.ByteString
      str_to_b = B.pack . map (fromIntegral . fromEnum)
  in fmap str_to_b . readProcess "nauty-planarg" ["-q"] . unlines

-- | 'plc_parse' of 'g6_planarg' of 'G6.g_to_g6'
g_to_plc :: [T.G] -> IO [PLC]
g_to_plc g = fmap plc_parse (G6.g_to_g6 g >>= g6_planarg)

-- | Run "nauty-planarg" to translate named G6 file to named PL file.
g6_to_pl_wr :: FilePath -> FilePath -> IO ()
g6_to_pl_wr g6_fn pl_fn = callProcess "nauty-planarg" ["-q","-p",g6_fn,pl_fn]
