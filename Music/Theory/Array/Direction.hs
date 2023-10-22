-- | Directions in an array.
module Music.Theory.Array.Direction where

import Data.List {- base -}
import Data.Maybe {- base -}

import Music.Theory.Geometry.Vector {- hmt-base -}

import qualified Music.Theory.Array.Cell_Ref as Cell_Ref {- hmt-base -}
import qualified Music.Theory.List as List {- hmt-base -}

-- * Loc / Vec

-- | (column,row)
type Loc n = V2 n

-- | (Δcolumn,Δrow), rows /descend/, ie. down is positive, up is negative.
type Vec n = V2 n

{- | Segment 'Vec' into a sequence of unit steps.

>>> map segment_vec [(0,0),(0,1),(-1,1),(-2,3)]
[[(0,0)],[(0,1)],[(0,1),(-1,0)],[(0,1),(0,1),(0,1),(-1,0),(-1,0)]]
-}
segment_vec :: Integral n => Vec n -> [Vec n]
segment_vec v =
    case v of
      (0,0) -> [v]
      (c,r) -> genericReplicate (abs r) (0,signum r) ++ genericReplicate (abs c) (signum c,0)

derive_vec :: Num n => Loc n -> Loc n -> Vec n
derive_vec (c1,r1) (c2,r2) = (c2 - c1,r2 - r1)

unfold_path :: Num n => Loc n -> [Vec n] -> [Loc n]
unfold_path = scanl v2_add

-- * Direction (non-diagonal)

type Direction_S = String

-- | Directions are D=down, L=left, R=right, U=up.
is_direction :: String -> Bool
is_direction = (`elem` "DLRU.") . List.head_err

type Direction_C = Char

-- | Reads either S|D W|L E|R N|U, reverse lookup gives SWEN. A period
-- indicates (0,0). S=south, W=west, E=east, N=north.
direction_char_to_vector_tbl :: Num n => [(Direction_C,Vec n)]
direction_char_to_vector_tbl =
    [('.',(0,0))
    ,('S',(0,1)),('W',(-1,0)),('E',(1,0)),('N',(0,-1))
    ,('D',(0,1)),('L',(-1,0)),('R',(1,0)),('U',(0,-1))]

{- | Letter to vector

>>> map direction_char_to_vector "LU"
[(-1,0),(0,-1)]
-}
direction_char_to_vector :: Num n => Direction_C -> Vec n
direction_char_to_vector d = fromMaybe (error "dir?") $ lookup d direction_char_to_vector_tbl

{- | Direction to vector

>>> map direction_to_vector (words "U D L R UL DR LL LU")
[(0,-1),(0,1),(-1,0),(1,0),(-1,-1),(1,1),(-2,0),(-1,-1)]
-}
direction_to_vector :: Num n => [Direction_C] -> Vec n
direction_to_vector = v2_sum . map direction_char_to_vector

vector_to_direction_char :: (Eq n, Num n) => Vec n -> Direction_C
vector_to_direction_char v =
    let r = List.reverse_lookup v direction_char_to_vector_tbl
    in fromMaybe (error "vec->dir?") r

-- | Direction sequence to cell references.
dir_seq_to_cell_seq :: (String,[String]) -> [String]
dir_seq_to_cell_seq (l,v) =
    let p = map direction_to_vector v
        c = Cell_Ref.parse_cell_index l
    in map (Cell_Ref.cell_ref_pp . Cell_Ref.index_to_cell) (unfold_path c p)

