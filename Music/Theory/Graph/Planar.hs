-- | <https://users.cecs.anu.edu.au/~bdm/plantri/plantri-guide.txt>
module Music.Theory.Graph.Planar where

import Data.List.Split {- split -}

import qualified Data.ByteString as B {- bytestring -}

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

-- | Group PLC data into PLC structure.
plc_group :: Int -> [Int] -> PLC
plc_group k i =
  let c = endBy [0] i
  in if length c == k then (k,c) else error "plc_group?"

-- | Segment input data into sequence of PLC.
plc_segment :: [Int] -> [PLC]
plc_segment i =
  case i of
    [] -> []
    k:j -> case plc_scanner k j of
             (r,[]) -> [plc_group k r]
             (r,l) -> plc_group k r : plc_segment l

-- | Load sequence of PLC from binary PLANAR-CODE file.
plc_load :: FilePath -> IO [PLC]
plc_load fn = do
  b <- B.readFile fn
  if plc_header b == plc_header_txt
    then return (plc_segment (plc_data b))
    else error "plc_load?"

-- | Translate 'PLC' into 'T.G'
plc_to_g :: PLC -> T.G
plc_to_g (k,n) =
  let v = [0 .. k - 1]
      f (i,j) = map (\x -> (i,x - 1)) j
      g (i,j) = i <= j
  in (v,filter g (concatMap f (zip v n)))
