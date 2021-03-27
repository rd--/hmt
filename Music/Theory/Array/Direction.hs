-- | Directions in an array.
module Music.Theory.Array.Direction where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.Array.Cell_Ref as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}

-- * LOC / VEC

-- | (column,row)
type LOC n = (n,n)

-- | (Δcolumn,Δrow), rows /descend/, ie. down is positive, up is negative.
type VEC n = (n,n)

vector_add :: Num n => VEC n -> VEC n -> VEC n
vector_add (c1,r1) (c2,r2) = (c1 + c2,r1 + r2)

vector_sub :: Num n => VEC n -> VEC n -> VEC n
vector_sub (c1,r1) (c2,r2) = (c1 - c2,r1 - r2)

vector_sum :: Num n => [VEC n] -> VEC n
vector_sum = foldl1 vector_add

apply_vec :: Num n => LOC n -> VEC n -> LOC n
apply_vec (c,r) (dc,dr) = (c + dc,r + dr)

-- | Segment 'VEC' into a sequence of unit steps.
--
-- > let r = [[(0,0)],[(0,1)],[(0,1),(-1,0)],[(0,1),(0,1),(0,1),(-1,0),(-1,0)]]
-- > in map segment_vec [(0,0),(0,1),(-1,1),(-2,3)] == r
segment_vec :: Integral n => VEC n -> [VEC n]
segment_vec v =
    case v of
      (0,0) -> [v]
      (c,r) -> genericReplicate (abs r) (0,signum r) ++ genericReplicate (abs c) (signum c,0)

derive_vec :: Num n => LOC n -> LOC n -> VEC n
derive_vec (c1,r1) (c2,r2) = (c2 - c1,r2 - r1)

unfold_path :: Num n => LOC n -> [VEC n] -> [LOC n]
unfold_path = scanl apply_vec

-- * DIRECTION (non-diagonal)

type DIRECTION_S = String

-- | Directions are D=down, L=left, R=right, U=up.
is_direction :: String -> Bool
is_direction = (`elem` "DLRU.") . head

type DIRECTION_C = Char

-- | Reads either S|D W|L E|R N|U, reverse lookup gives SWEN. A period
-- indicates (0,0). S=south, W=west, E=east, N=north.
direction_char_to_vector_tbl :: Num n => [(DIRECTION_C,VEC n)]
direction_char_to_vector_tbl =
    [('.',(0,0))
    ,('S',(0,1)),('W',(-1,0)),('E',(1,0)),('N',(0,-1))
    ,('D',(0,1)),('L',(-1,0)),('R',(1,0)),('U',(0,-1))]

-- > map direction_char_to_vector "LU"
direction_char_to_vector :: Num n => DIRECTION_C -> VEC n
direction_char_to_vector d = fromMaybe (error "dir?") $ lookup d direction_char_to_vector_tbl

-- > let r = [(0,-1),(0,1),(-1,0),(1,0),(-1,-1),(1,1),(-2,0),(-1,-1)]
-- > in map direction_to_vector (words "U D L R UL DR LL LU") == r
direction_to_vector :: Num n => [DIRECTION_C] -> VEC n
direction_to_vector = vector_sum . map direction_char_to_vector

vector_to_direction_char :: (Eq n, Num n) => VEC n -> DIRECTION_C
vector_to_direction_char v =
    let r = T.reverse_lookup v direction_char_to_vector_tbl
    in fromMaybe (error "vec->dir?") r

-- | Direction sequence to cell references.
dir_seq_to_cell_seq :: (String,[String]) -> [String]
dir_seq_to_cell_seq (l,v) =
    let p = map direction_to_vector v
        c = T.parse_cell_index l
    in map (T.cell_ref_pp . T.index_to_cell) (unfold_path c p)

