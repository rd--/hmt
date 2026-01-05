{- | Functions for drawing grid and table structures common in music
theory and in compositions such as Morton Feldman's durational
/grid/ music of the 1950's.
-}
module Music.Theory.Diagram.Grid where

import Data.Maybe {- base -}
import Text.Printf {- base -}

import Music.Theory.Geometry.Vector {- hmt-base -}

import qualified Text.Html.Minus as Html {- html-minus -}

-- * Grid

-- | Point given as pair of 'R'.
type Pt = V2 Double

-- | Vector (ie. not point)
type Vec = V2 Double

-- | Cell location as row and column indices.
type Loc = V2 Int

{- | Given /(x,y)/ upper-left co-ordinate of grid, /(w,h)/ cell
dimensions, and /(r,c)/ grid dimensions, make array of upper-left
co-ordinates of cells.

>>> grid (10,10) (50,10) (2,2)
[[(10,10),(60,10)],[(10,20),(60,20)]]
-}
grid :: (Enum a, Num a) => V2 a -> V2 a -> V2 Int -> [[V2 a]]
grid (x, y) (w, h) (nr, nc) =
  let xs = take nc [x, x + w ..]
      ys = take nr [y, y + h ..]
  in map (zip xs . repeat) ys

{- | Variant on 'grid' that constructs a single point given (row,column) index.

>>> map (grid_pt (10,10) (50,10)) [(0,0),(1,1)]
[(10.0,10.0),(60.0,20.0)]
-}
grid_pt :: Pt -> Vec -> Loc -> Pt
grid_pt (x, y) (w, h) (r, c) =
  let r' = fromIntegral r
      c' = fromIntegral c
  in (x + c' * w, y + r' * h)

{- | Displace 'P' (pointwise addition).

>>> displace (2,3) (1,1)
(3.0,4.0)
-}
displace :: Vec -> Pt -> Pt
displace (dx, dy) (x, y) = (x + dx, y + dy)

-- | Make a bounding box from /row/ and /column/ dimensions.
mk_bbox :: V2 Int -> Vec
mk_bbox (r, c) =
  let f n = (fromIntegral n + 2) * 10
  in (f c, f r)

-- * Table (Html)

-- | A table cell is an 'Html.Attr' and 'Html.Content' duple.
type Table_Cell = ([Html.Attr], [Html.Content])

-- | A table caption.
type Caption = [Html.Content]

-- | Table of row order 'Table_Cell's.
type Html_Table = (Caption, [[Table_Cell]])

-- | Construct a 'Table' with one 'Html.Content' per cell.
simple_table :: Caption -> [[Html.Content]] -> Html_Table
simple_table c z = (c, map (map (\x -> ([], [x]))) z)

{- | Construct a 'Table' with one 'Html.Content' per cell, and an
associated class.
-}
simple_table_class :: Caption -> [[(String, Html.Content)]] -> Html_Table
simple_table_class c z = (c, map (map (\(nm, x) -> ([Html.class_attr nm], [x]))) z)

-- | A function from @(row,column)@ to 'Maybe' 'Table_Cell'
type Build_f = ((Int, Int) -> Maybe Table_Cell)

{- | Build a table of @(rows,columns)@ dimensions given a builder
function.  If the function is 'Nothing' the cell is skipped, because
another cell has claimed it's locations with 'Html.colspan' or
'Html.rowspan'.
-}
build_table_m :: Caption -> (Int, Int) -> Build_f -> Html_Table
build_table_m c (m, n) f =
  let mk_row i = mapMaybe (\j -> f (i, j)) [0 .. n - 1]
  in (c, map mk_row [0 .. m - 1])

{- | Build a table of @(rows,columns)@ dimensions given a function
from @(row,column)@ to 'Table_Cell'.
-}
build_table :: Caption -> (Int, Int) -> ((Int, Int) -> Table_Cell) -> Html_Table
build_table c (m, n) f = build_table_m c (m, n) (Just . f)

-- | Render 'Table' as @Html@ table.
table :: Html_Table -> Html.Content
table (c, z) =
  let mk_r = Html.tr [] . map (uncurry Html.td)
  in Html.table [] [Html.caption [] c, Html.tbody [] (map mk_r z)]

-- | A set of related tables.
type Html_Table_Set = [Html_Table]

-- | Render a 'Table_Set's in a @div@ with class @table-set@.
table_set :: Html_Table_Set -> Html.Content
table_set = Html.div [Html.class_attr "table-set"] . map table

-- | Render set of 'Table_Set's as @Html@.
page :: Maybe FilePath -> [Html_Table_Set] -> String
page css xs = do
  let tb = map table_set xs
      bd = Html.body [Html.class_attr "table-page"] tb
      css' = Html.link_css "all" (fromMaybe "css/grid.css" css)
      hd =
        Html.head
          []
          [ Html.title [] [Html.cdata "Music.Theory.Diagram.Grid"]
          , Html.meta [Html.charset "utf-8"]
          , css'
          ]
      e = Html.html [Html.lang "en"] [hd, bd]
  Html.renderHtml5_pp e

-- | Write set of 'Table_Set's to @Html@ file.
to_html :: FilePath -> Maybe FilePath -> [Html_Table_Set] -> IO ()
to_html o_fn css = writeFile o_fn . page css

-- * Colour grid

type U8 = Int
type RGB24 = (U8, U8, U8)

clr_set_attr :: RGB24 -> Html.Attr
clr_set_attr (r, g, b) = Html.style_attr (printf "background-color: rgb(%d,%d,%d)" r g b)

clr_ix_to_cell :: Int -> [RGB24] -> (Int, Int) -> Table_Cell
clr_ix_to_cell nc clr_seq (r, c) =
  let clr = clr_seq !! (r * nc + c)
  in ([clr_set_attr clr], [])

clr_gen_tbl :: (Int, Int) -> [RGB24] -> Html.Content
clr_gen_tbl (r, c) = table . build_table [] (r, c) . clr_ix_to_cell c
