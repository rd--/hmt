-- | <http://www.unicode.org/charts/PDF/U1D100.pdf>
--
-- <http://unicode.org/Public/8.0.0/ucd/UnicodeData.txt>
module Music.Theory.Unicode where

import Data.List {- base -}
import Numeric {- base -}

import qualified Text.CSV.Lazy.String as C {- lazy-csv -}

import qualified Music.Theory.IO as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Read as T {- hmt -}

type Unicode_Index = Int
type Unicode_Range = (Unicode_Index,Unicode_Index)
type Unicode_Point = (Unicode_Index,String)
type Unicode_Table = [Unicode_Point]

-- > putStrLn$ map (toEnum . fst) (concat unicode)
unicode :: [Unicode_Table]
unicode = [accidentals,notes,rests,clefs]

-- > putStrLn$ unicode_table_hs (unicode_table_block accidentals_rng tbl)
accidentals_rng :: Unicode_Range
accidentals_rng = (0x1D12A,0x1D133)

-- | UNICODE accidental symbols.
--
-- > let r = "𝄪𝄫𝄬𝄭𝄮𝄯𝄰𝄱𝄲𝄳" in map (toEnum . fst) accidentals == r
accidentals :: Unicode_Table
accidentals =
    [(0x1D12A,"MUSICAL SYMBOL DOUBLE SHARP")
    ,(0x1D12B,"MUSICAL SYMBOL DOUBLE FLAT")
    ,(0x1D12C,"MUSICAL SYMBOL FLAT UP")
    ,(0x1D12D,"MUSICAL SYMBOL FLAT DOWN")
    ,(0x1D12E,"MUSICAL SYMBOL NATURAL UP")
    ,(0x1D12F,"MUSICAL SYMBOL NATURAL DOWN")
    ,(0x1D130,"MUSICAL SYMBOL SHARP UP")
    ,(0x1D131,"MUSICAL SYMBOL SHARP DOWN")
    ,(0x1D132,"MUSICAL SYMBOL QUARTER TONE SHARP")
    ,(0x1D133,"MUSICAL SYMBOL QUARTER TONE FLAT")]

-- > putStrLn$ unicode_table_hs (unicode_table_block notes_rng tbl)
notes_rng :: Unicode_Range
notes_rng = (0x1D15C,0x1D164)

-- | UNICODE note duration symbols.
--
-- > let r = "𝅜𝅝𝅗𝅥𝅘𝅥𝅘𝅥𝅮𝅘𝅥𝅯𝅘𝅥𝅰𝅘𝅥𝅱𝅘𝅥𝅲" in map (toEnum . fst) notes == r
notes :: Unicode_Table
notes =
    [(0x1D15C,"MUSICAL SYMBOL BREVE")
    ,(0x1D15D,"MUSICAL SYMBOL WHOLE NOTE")
    ,(0x1D15E,"MUSICAL SYMBOL HALF NOTE")
    ,(0x1D15F,"MUSICAL SYMBOL QUARTER NOTE")
    ,(0x1D160,"MUSICAL SYMBOL EIGHTH NOTE")
    ,(0x1D161,"MUSICAL SYMBOL SIXTEENTH NOTE")
    ,(0x1D162,"MUSICAL SYMBOL THIRTY-SECOND NOTE")
    ,(0x1D163,"MUSICAL SYMBOL SIXTY-FOURTH NOTE")
    ,(0x1D164,"MUSICAL SYMBOL ONE HUNDRED TWENTY-EIGHTH NOTE")]

-- > putStrLn$ unicode_table_hs (unicode_table_block rests_rng tbl)
rests_rng :: Unicode_Range
rests_rng = (0x1D13B,0x1D142)

-- | UNICODE rest symbols.
--
-- > let r = "𝄻𝄼𝄽𝄾𝄿𝅀𝅁𝅂" in map (toEnum . fst) rests == r
rests :: Unicode_Table
rests =
    [(0x1D13B,"MUSICAL SYMBOL WHOLE REST")
    ,(0x1D13C,"MUSICAL SYMBOL HALF REST")
    ,(0x1D13D,"MUSICAL SYMBOL QUARTER REST")
    ,(0x1D13E,"MUSICAL SYMBOL EIGHTH REST")
    ,(0x1D13F,"MUSICAL SYMBOL SIXTEENTH REST")
    ,(0x1D140,"MUSICAL SYMBOL THIRTY-SECOND REST")
    ,(0x1D141,"MUSICAL SYMBOL SIXTY-FOURTH REST")
    ,(0x1D142,"MUSICAL SYMBOL ONE HUNDRED TWENTY-EIGHTH REST")]

-- > putStrLn$ unicode_table_hs (unicode_table_block clefs_rng tbl)
clefs_rng :: Unicode_Range
clefs_rng = (0x1D11E,0x1D126)

-- | UNICODE clef symbols.
--
-- > let r = "𝄞𝄟𝄠𝄡𝄢𝄣𝄤𝄥𝄦" in map (toEnum . fst) clefs == r
clefs :: Unicode_Table
clefs =
    [(0x1D11E,"MUSICAL SYMBOL G CLEF")
    ,(0x1D11F,"MUSICAL SYMBOL G CLEF OTTAVA ALTA")
    ,(0x1D120,"MUSICAL SYMBOL G CLEF OTTAVA BASSA")
    ,(0x1D121,"MUSICAL SYMBOL C CLEF")
    ,(0x1D122,"MUSICAL SYMBOL F CLEF")
    ,(0x1D123,"MUSICAL SYMBOL F CLEF OTTAVA ALTA")
    ,(0x1D124,"MUSICAL SYMBOL F CLEF OTTAVA BASSA")
    ,(0x1D125,"MUSICAL SYMBOL DRUM CLEF-1")
    ,(0x1D126,"MUSICAL SYMBOL DRUM CLEF-2")]

-- > putStrLn$ unicode_table_hs (unicode_table_block notehead_rng tbl)
notehead_rng :: Unicode_Range
notehead_rng = (0x1D143,0x1D15B)

-- | UNICODE notehead symbols.
--
-- > let r = "𝅃𝅄𝅅𝅆𝅇𝅈𝅉𝅊𝅋𝅌𝅍𝅎𝅏𝅐𝅑𝅒𝅓𝅔𝅕𝅖𝅗𝅘𝅙𝅚𝅛" in map (toEnum . fst) noteheads == r
noteheads :: Unicode_Table
noteheads =
    [(0x1d143,"MUSICAL SYMBOL X NOTEHEAD")
    ,(0x1d144,"MUSICAL SYMBOL PLUS NOTEHEAD")
    ,(0x1d145,"MUSICAL SYMBOL CIRCLE X NOTEHEAD")
    ,(0x1d146,"MUSICAL SYMBOL SQUARE NOTEHEAD WHITE")
    ,(0x1d147,"MUSICAL SYMBOL SQUARE NOTEHEAD BLACK")
    ,(0x1d148,"MUSICAL SYMBOL TRIANGLE NOTEHEAD UP WHITE")
    ,(0x1d149,"MUSICAL SYMBOL TRIANGLE NOTEHEAD UP BLACK")
    ,(0x1d14a,"MUSICAL SYMBOL TRIANGLE NOTEHEAD LEFT WHITE")
    ,(0x1d14b,"MUSICAL SYMBOL TRIANGLE NOTEHEAD LEFT BLACK")
    ,(0x1d14c,"MUSICAL SYMBOL TRIANGLE NOTEHEAD RIGHT WHITE")
    ,(0x1d14d,"MUSICAL SYMBOL TRIANGLE NOTEHEAD RIGHT BLACK")
    ,(0x1d14e,"MUSICAL SYMBOL TRIANGLE NOTEHEAD DOWN WHITE")
    ,(0x1d14f,"MUSICAL SYMBOL TRIANGLE NOTEHEAD DOWN BLACK")
    ,(0x1d150,"MUSICAL SYMBOL TRIANGLE NOTEHEAD UP RIGHT WHITE")
    ,(0x1d151,"MUSICAL SYMBOL TRIANGLE NOTEHEAD UP RIGHT BLACK")
    ,(0x1d152,"MUSICAL SYMBOL MOON NOTEHEAD WHITE")
    ,(0x1d153,"MUSICAL SYMBOL MOON NOTEHEAD BLACK")
    ,(0x1d154,"MUSICAL SYMBOL TRIANGLE-ROUND NOTEHEAD DOWN WHITE")
    ,(0x1d155,"MUSICAL SYMBOL TRIANGLE-ROUND NOTEHEAD DOWN BLACK")
    ,(0x1d156,"MUSICAL SYMBOL PARENTHESIS NOTEHEAD")
    ,(0x1d157,"MUSICAL SYMBOL VOID NOTEHEAD")
    ,(0x1d158,"MUSICAL SYMBOL NOTEHEAD BLACK")
    ,(0x1d159,"MUSICAL SYMBOL NULL NOTEHEAD")
    ,(0x1d15a,"MUSICAL SYMBOL CLUSTER NOTEHEAD WHITE")
    ,(0x1d15b,"MUSICAL SYMBOL CLUSTER NOTEHEAD BLACK")]

-- > putStrLn$ map toEnum [0x1D15F,0x1D16D]
augmentation_dot :: Unicode_Point
augmentation_dot = (0x1D16D, "MUSICAL SYMBOL COMBINING AUGMENTATION DOT")

-- * Blocks

type Unicode_Block = (Unicode_Range,String)

-- > putStrLn$ unicode_table_hs (concatMap (flip unicode_table_block tbl . fst) unicode_blocks)
unicode_blocks :: [Unicode_Block]
unicode_blocks =
    [((0x1B00,0x1B7F),"Balinese")
    ,((0x2200,0x22FF),"Mathematical Operators")
    ,((0x25A0,0x25FF),"Geometric Shapes")
    ,((0x1D000,0x1D0FF),"Byzantine Musical Symbols")
    ,((0x1D100,0x1D1FF),"Musical Symbols")
    ,((0x1D200,0x1D24F),"Ancient Greek Musical Notation")]

-- * Table

-- > let fn = "/home/rohan/data/unicode.org/Public/8.0.0/ucd/UnicodeData.txt"
-- > tbl <- unicode_data_table_read fn
-- > length tbl == 29215
unicode_data_table_read :: FilePath -> IO Unicode_Table
unicode_data_table_read fn = do
  s <- T.read_file_utf8 fn
  let t = C.fromCSVTable (C.csvTable (C.parseDSV False ';' s))
      f x = (T.read_hex_err (x !! 0),x !! 1)
  return (map f t)

unicode_table_block :: (Int,Int) -> Unicode_Table -> Unicode_Table
unicode_table_block (l,r) = takeWhile ((<= r) . fst) . dropWhile ((< l) . fst)

unicode_point_hs :: Unicode_Point -> String
unicode_point_hs (n,s) = concat ["(0x",showHex n "",",\"",s,"\")"]

unicode_table_hs :: Unicode_Table -> String
unicode_table_hs = T.bracket ('[',']') . intercalate "," . map unicode_point_hs
