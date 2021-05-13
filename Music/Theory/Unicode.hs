-- | <http://www.unicode.org/charts/PDF/U1D100.pdf>
--
-- These symbols are in <http://www.gnu.org/software/freefont/>,
-- debian=ttf-freefont.
module Music.Theory.Unicode where

import Data.Char {- base -}
import Data.List {- base -}
import Numeric {- base -}

import qualified Text.CSV.Lazy.String as C {- lazy-csv -}

import qualified Music.Theory.IO as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Read as T {- hmt -}

-- * Non-music

-- | Unicode non breaking hypen character.
--
-- > non_breaking_hypen == '‑'
non_breaking_hypen :: Char
non_breaking_hypen = toEnum 0x2011

-- | Unicode non breaking space character.
--
-- > non_breaking_space == ' '
non_breaking_space :: Char
non_breaking_space = toEnum 0x00A0

-- | Unicode interpunct.
--
-- > middle_dot == '·'
middle_dot :: Char
middle_dot = toEnum 0x00B7

-- | The superscript variants of the digits 0-9
superscript_digits :: [Char]
superscript_digits = "⁰¹²³⁴⁵⁶⁷⁸⁹"

-- | Map 'show' of 'Int' to 'superscript_digits'.
--
-- > unwords (map int_show_superscript [0,12,345,6789]) == "⁰ ¹² ³⁴⁵ ⁶⁷⁸⁹"
int_show_superscript :: Int -> String
int_show_superscript = map ((superscript_digits !!) . digitToInt) . show

-- | The subscript variants of the digits 0-9
subscript_digits :: [Char]
subscript_digits = "₀₁₂₃₄₅₆₇₈₉"

-- | The combining over line character.
--
-- > ['1',combining_overline] == "1̅"
-- > ['A',combining_overline] == "A̅"
combining_overline :: Char
combining_overline = toEnum 0x0305

-- | Add 'combining_overline' to each 'Char'.
--
-- > overline "1234" == "1̅2̅3̅4̅"
overline :: String -> String
overline = let f x = [x,combining_overline] in concatMap f

-- | The combining under line character.
--
-- > ['1',combining_underline] == "1̲"
combining_underline :: Char
combining_underline = toEnum 0x0332

-- | Add 'combining_underline' to each 'Char'.
--
-- > underline "1234" == "1̲2̲3̲4̲"
underline :: String -> String
underline = let f x = [x,combining_underline] in concatMap f

-- * Table

type Unicode_Index = Int
type Unicode_Name = String
type Unicode_Range = (Unicode_Index,Unicode_Index)
type Unicode_Point = (Unicode_Index,Unicode_Name)
type Unicode_Table = [Unicode_Point]

{- | <http://unicode.org/Public/11.0.0/ucd/UnicodeData.txt>

> let fn = "/home/rohan/data/unicode.org/Public/11.0.0/ucd/UnicodeData.txt"
> tbl <- unicode_data_table_read fn
> length tbl == 32292
> T.reverse_lookup_err "MIDDLE DOT" tbl == 0x00B7
> putStrLn $ unwords $ map (\(n,x) -> toEnum n : x) $ filter (\(_,x) -> "EMPTY SET" `isInfixOf` x) tbl
> T.lookup_err 0x22C5 tbl == "DOT OPERATOR"
-}
unicode_data_table_read :: FilePath -> IO Unicode_Table
unicode_data_table_read fn = do
  s <- T.read_file_utf8 fn
  let t = C.fromCSVTable (C.csvTable (C.parseDSV False ';' s))
      f x = (T.read_hex_err (x !! 0),x !! 1)
  return (map f t)

unicode_table_block :: (Unicode_Index,Unicode_Index) -> Unicode_Table -> Unicode_Table
unicode_table_block (l,r) = takeWhile ((<= r) . fst) . dropWhile ((< l) . fst)

unicode_point_hs :: Unicode_Point -> String
unicode_point_hs (n,s) = concat ["(0x",showHex n "",",\"",s,"\")"]

unicode_table_hs :: Unicode_Table -> String
unicode_table_hs = T.bracket ('[',']') . intercalate "," . map unicode_point_hs

-- * Music

-- > putStrLn$ map (toEnum . fst) (concat music_tbl)
music_tbl :: [Unicode_Table]
music_tbl = [barlines_tbl,accidentals_tbl,notes_tbl,rests_tbl,clefs_tbl]

-- > putStrLn$ concatMap (unicode_table_hs . flip unicode_table_block tbl) accidentals_rng_set
accidentals_rng_set :: [Unicode_Range]
accidentals_rng_set = [(0x266D,0x266F),(0x1D12A,0x1D133)]

-- > putStrLn$ unicode_table_hs (unicode_table_block barlines_rng tbl)
barlines_rng :: Unicode_Range
barlines_rng = (0x1D100,0x1D105)

-- | UNICODE barline symbols.
--
-- > let r = "𝄀𝄁𝄂𝄃𝄄𝄅" in map (toEnum . fst) barlines_tbl == r
barlines_tbl :: Unicode_Table
barlines_tbl =
  [(0x1D100,"MUSICAL SYMBOL SINGLE BARLINE")
  ,(0x1D101,"MUSICAL SYMBOL DOUBLE BARLINE")
  ,(0x1D102,"MUSICAL SYMBOL FINAL BARLINE")
  ,(0x1D103,"MUSICAL SYMBOL REVERSE FINAL BARLINE")
  ,(0x1D104,"MUSICAL SYMBOL DASHED BARLINE")
  ,(0x1D105,"MUSICAL SYMBOL SHORT BARLINE")]

-- | UNICODE accidental symbols.
--
-- > let r = "♭♮♯𝄪𝄫𝄬𝄭𝄮𝄯𝄰𝄱𝄲𝄳" in map (toEnum . fst) accidentals_tbl == r
accidentals_tbl :: Unicode_Table
accidentals_tbl =
    [(0x266D,"MUSIC FLAT SIGN")
    ,(0x266E,"MUSIC NATURAL SIGN")
    ,(0x266F,"MUSIC SHARP SIGN")
    ,(0x1D12A,"MUSICAL SYMBOL DOUBLE SHARP")
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
-- > let r = "𝅜𝅝𝅗𝅥𝅘𝅥𝅘𝅥𝅮𝅘𝅥𝅯𝅘𝅥𝅰𝅘𝅥𝅱𝅘𝅥𝅲" in map (toEnum . fst) notes_tbl == r
notes_tbl :: Unicode_Table
notes_tbl =
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
-- > let r = "𝄻𝄼𝄽𝄾𝄿𝅀𝅁𝅂" in map (toEnum . fst) rests_tbl == r
rests_tbl :: Unicode_Table
rests_tbl =
    [(0x1D13B,"MUSICAL SYMBOL WHOLE REST")
    ,(0x1D13C,"MUSICAL SYMBOL HALF REST")
    ,(0x1D13D,"MUSICAL SYMBOL QUARTER REST")
    ,(0x1D13E,"MUSICAL SYMBOL EIGHTH REST")
    ,(0x1D13F,"MUSICAL SYMBOL SIXTEENTH REST")
    ,(0x1D140,"MUSICAL SYMBOL THIRTY-SECOND REST")
    ,(0x1D141,"MUSICAL SYMBOL SIXTY-FOURTH REST")
    ,(0x1D142,"MUSICAL SYMBOL ONE HUNDRED TWENTY-EIGHTH REST")]

-- | Augmentation dot.
--
-- > map toEnum [0x1D15E,0x1D16D,0x1D16D] == "𝅗𝅥𝅭𝅭"
augmentation_dot :: Unicode_Point
augmentation_dot = (0x1D16D, "MUSICAL SYMBOL COMBINING AUGMENTATION DOT")

-- > putStrLn$ unicode_table_hs (unicode_table_block clefs_rng tbl)
clefs_rng :: Unicode_Range
clefs_rng = (0x1D11E,0x1D126)

-- | UNICODE clef symbols.
--
-- > let r = "𝄞𝄟𝄠𝄡𝄢𝄣𝄤𝄥𝄦" in map (toEnum . fst) clefs_tbl == r
clefs_tbl :: Unicode_Table
clefs_tbl =
    [(0x1D11E,"MUSICAL SYMBOL G CLEF")
    ,(0x1D11F,"MUSICAL SYMBOL G CLEF OTTAVA ALTA")
    ,(0x1D120,"MUSICAL SYMBOL G CLEF OTTAVA BASSA")
    ,(0x1D121,"MUSICAL SYMBOL C CLEF")
    ,(0x1D122,"MUSICAL SYMBOL F CLEF")
    ,(0x1D123,"MUSICAL SYMBOL F CLEF OTTAVA ALTA")
    ,(0x1D124,"MUSICAL SYMBOL F CLEF OTTAVA BASSA")
    ,(0x1D125,"MUSICAL SYMBOL DRUM CLEF-1")
    ,(0x1D126,"MUSICAL SYMBOL DRUM CLEF-2")]

-- > putStrLn$ unicode_table_hs (unicode_table_block noteheads_rng tbl)
noteheads_rng :: Unicode_Range
noteheads_rng = (0x1D143,0x1D15B)

-- | UNICODE notehead symbols.
--
-- > let r = "𝅃𝅄𝅅𝅆𝅇𝅈𝅉𝅊𝅋𝅌𝅍𝅎𝅏𝅐𝅑𝅒𝅓𝅔𝅕𝅖𝅗𝅘𝅙𝅚𝅛" in map (toEnum . fst) noteheads_tbl == r
noteheads_tbl :: Unicode_Table
noteheads_tbl =
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

-- > map toEnum [0x1D143,0x1D165] == "𝅃𝅥"
stem :: Unicode_Point
stem = (0x1D165, "MUSICAL SYMBOL COMBINING STEM")

-- > putStrLn$ unicode_table_hs (unicode_table_block dynamics_rng tbl)
dynamics_rng :: Unicode_Range
dynamics_rng = (0x1D18C,0x1D193)

-- > map (toEnum . fst) dynamics_tbl == "𝆌𝆍𝆎𝆏𝆐𝆑𝆒𝆓"
dynamics_tbl :: Unicode_Table
dynamics_tbl =
    [(0x1d18c,"MUSICAL SYMBOL RINFORZANDO")
    ,(0x1d18d,"MUSICAL SYMBOL SUBITO")
    ,(0x1d18e,"MUSICAL SYMBOL Z")
    ,(0x1d18f,"MUSICAL SYMBOL PIANO")
    ,(0x1d190,"MUSICAL SYMBOL MEZZO")
    ,(0x1d191,"MUSICAL SYMBOL FORTE")
    ,(0x1d192,"MUSICAL SYMBOL CRESCENDO")
    ,(0x1d193,"MUSICAL SYMBOL DECRESCENDO")]

-- > putStrLn$ unicode_table_hs (unicode_table_block articulations_rng tbl)
articulations_rng :: Unicode_Range
articulations_rng = (0x1D17B,0x1D18B)

-- > putStrLn (map (toEnum . fst) articulations_tbl :: String)
articulations_tbl :: Unicode_Table
articulations_tbl =
    [(0x1d17b,"MUSICAL SYMBOL COMBINING ACCENT")
    ,(0x1d17c,"MUSICAL SYMBOL COMBINING STACCATO")
    ,(0x1d17d,"MUSICAL SYMBOL COMBINING TENUTO")
    ,(0x1d17e,"MUSICAL SYMBOL COMBINING STACCATISSIMO")
    ,(0x1d17f,"MUSICAL SYMBOL COMBINING MARCATO")
    ,(0x1d180,"MUSICAL SYMBOL COMBINING MARCATO-STACCATO")
    ,(0x1d181,"MUSICAL SYMBOL COMBINING ACCENT-STACCATO")
    ,(0x1d182,"MUSICAL SYMBOL COMBINING LOURE")
    ,(0x1d183,"MUSICAL SYMBOL ARPEGGIATO UP")
    ,(0x1d184,"MUSICAL SYMBOL ARPEGGIATO DOWN")
    ,(0x1d185,"MUSICAL SYMBOL COMBINING DOIT")
    ,(0x1d186,"MUSICAL SYMBOL COMBINING RIP")
    ,(0x1d187,"MUSICAL SYMBOL COMBINING FLIP")
    ,(0x1d188,"MUSICAL SYMBOL COMBINING SMEAR")
    ,(0x1d189,"MUSICAL SYMBOL COMBINING BEND")
    ,(0x1d18a,"MUSICAL SYMBOL COMBINING DOUBLE TONGUE")
    ,(0x1d18b,"MUSICAL SYMBOL COMBINING TRIPLE TONGUE")]

-- * Math

ix_set_to_tbl :: Unicode_Table -> [Unicode_Index] -> Unicode_Table
ix_set_to_tbl tbl ix = zip ix (map (`T.lookup_err` tbl) ix)

-- | Unicode dot-operator.
--
-- > dot_operator == '⋅'
dot_operator :: Char
dot_operator = toEnum 0x22C5

-- | Math symbols outside of the math blocks.
--
-- > putStrLn (unicode_table_hs (ix_set_to_tbl tbl math_plain_ix))
math_plain_ix :: [Unicode_Index]
math_plain_ix = [0x00D7,0x00F7]

-- > map (toEnum . fst) math_plain_tbl == "×÷"
math_plain_tbl :: Unicode_Table
math_plain_tbl = [(0xd7,"MULTIPLICATION SIGN"),(0xf7,"DIVISION SIGN")]

-- * Blocks

type Unicode_Block = (Unicode_Range,String)

-- > putStrLn$ unicode_table_hs (concatMap (flip unicode_table_block tbl . fst) unicode_blocks)
unicode_blocks :: [Unicode_Block]
unicode_blocks =
    [((0x01B00,0x01B7F),"Balinese")
    ,((0x02200,0x022FF),"Mathematical Operators")
    ,((0x025A0,0x025FF),"Geometric Shapes")
    ,((0x027C0,0x027EF),"Miscellaneous Mathematical Symbols-A")
    ,((0x027F0,0x027FF),"Supplemental Arrows-A")
    ,((0x02800,0x028FF),"Braille Patterns")
    ,((0x02900,0x0297F),"Supplemental Arrows-B")
    ,((0x02980,0x029FF),"Miscellaneous Mathematical Symbols-B")
    ,((0x02A00,0x02AFF),"Supplemental Mathematical Operators")
    ,((0x1D000,0x1D0FF),"Byzantine Musical Symbols")
    ,((0x1D100,0x1D1FF),"Musical Symbols")
    ,((0x1D200,0x1D24F),"Ancient Greek Musical Notation")
    ]

-- * BAGUA, EIGHT TRI-GRAMS

-- | Bagua tri-grams.
--
-- > putStrLn $ unicode_table_hs (unicode_table_block (fst bagua) tbl)
bagua :: Unicode_Block
bagua = ((0x02630,0x02637),"BAGUA")

{- | Table of eight tri-grams.

HEAVEN,乾,Qián,☰,111
LAKE,兌,Duì,☱,110
FIRE,離,Lí,☲,101
THUNDER,震,Zhèn,☳,100
WIND,巽,Xùn,☴,011
WATER,坎,Kǎn,☵,010
MOUNTAIN,艮,Gèn,☶,001
EARTH,坤,Kūn,☷,000

-}
bagua_tbl :: Unicode_Table
bagua_tbl =
  [(0x2630,"TRIGRAM FOR HEAVEN")
  ,(0x2631,"TRIGRAM FOR LAKE")
  ,(0x2632,"TRIGRAM FOR FIRE")
  ,(0x2633,"TRIGRAM FOR THUNDER")
  ,(0x2634,"TRIGRAM FOR WIND")
  ,(0x2635,"TRIGRAM FOR WATER")
  ,(0x2636,"TRIGRAM FOR MOUNTAIN")
  ,(0x2637,"TRIGRAM FOR EARTH")]

-- * YIJING (I-CHING), SIXTY-FOUR HEXAGRAMS

-- | Yijing hexagrams in King Wen sequence.
--
-- > putStrLn $ unicode_table_hs (unicode_table_block (fst yijing) tbl)
yijing :: Unicode_Block
yijing = ((0x04DC0,0x04DFF),"YIJING")

{- | Yijing hexagrams in King Wen sequence.

䷀,乾,qián,111,111
䷁,坤,kūn,000,000
䷂,屯,chún,100,010
䷃,蒙,méng,010,001
䷄,需,xū,111,010
䷅,訟,sòng,010,111
䷆,師,shī,010,000
䷇,比,bǐ,000,010
䷈,小畜,xiǎo chù,111,011
䷉,履,lǚ,110,111
䷊,泰,tài,111,000
䷋,否,pǐ,000,111
䷌,同人,tóng rén,101,111
䷍,大有,dà yǒu,111,101
䷎,謙,qiān,001,000
䷏,豫,yù,000,100
䷐,隨,suí,100,110
䷑,蠱,gŭ,011,001
䷒,臨,lín,110,000
䷓,觀,guān,000,011
䷔,噬嗑,shì kè,100,101
䷕,賁,bì,101,001
䷖,剝,bō,000,001
䷗,復,fù,100,000
䷘,無妄,wú wàng,100,111
䷙,大畜,dà chù,111,001
䷚,頤,yí,100,001
䷛,大過,dà guò,011,110
䷜,坎,kǎn,010,010
䷝,離,lí,101,101
䷞,咸,xián,001,110
䷟,恆,héng,011,100
䷠,遯,dùn,001,111
䷡,大壯,dà zhuàng,111,100
䷢,晉,jìn,000,101
䷣,明夷,míng yí,101,000
䷤,家人,jiā rén,101,011
䷥,睽,kuí,110,101
䷦,蹇,jiǎn,001,010
䷧,解,xiè,010,100
䷨,損,sǔn,110,001
䷩,益,yì,100,011
䷪,夬,guài,111,110
䷫,姤,gòu,011,111
䷬,萃,cuì,000,110
䷭,升,shēng,011,000
䷮,困,kùn,010,110
䷯,井,jǐng,011,010
䷰,革,gé,101,110
䷱,鼎,dǐng,011,101
䷲,震,zhèn,100,100
䷳,艮,gèn,001,001
䷴,漸,jiàn,001,011
䷵,歸妹,guī mèi,110,100
䷶,豐,fēng,101,100
䷷,旅,lǚ,001,101
䷸,巽,xùn,011,011
䷹,兌,duì,110,110
䷺,渙,huàn,010,011
䷻,節,jié,110,010
䷼,中孚,zhōng fú,110,011
䷽,小過,xiǎo guò,001,110
䷾,既濟,jì jì,101,010
䷿,未濟,wèi jì,010,101
-}
yijing_tbl :: Unicode_Table
yijing_tbl =
  [(0x4dc0,"HEXAGRAM FOR THE CREATIVE HEAVEN")
  ,(0x4dc1,"HEXAGRAM FOR THE RECEPTIVE EARTH")
  ,(0x4dc2,"HEXAGRAM FOR DIFFICULTY AT THE BEGINNING")
  ,(0x4dc3,"HEXAGRAM FOR YOUTHFUL FOLLY")
  ,(0x4dc4,"HEXAGRAM FOR WAITING")
  ,(0x4dc5,"HEXAGRAM FOR CONFLICT")
  ,(0x4dc6,"HEXAGRAM FOR THE ARMY")
  ,(0x4dc7,"HEXAGRAM FOR HOLDING TOGETHER")
  ,(0x4dc8,"HEXAGRAM FOR SMALL TAMING")
  ,(0x4dc9,"HEXAGRAM FOR TREADING")
  ,(0x4dca,"HEXAGRAM FOR PEACE")
  ,(0x4dcb,"HEXAGRAM FOR STANDSTILL")
  ,(0x4dcc,"HEXAGRAM FOR FELLOWSHIP")
  ,(0x4dcd,"HEXAGRAM FOR GREAT POSSESSION")
  ,(0x4dce,"HEXAGRAM FOR MODESTY")
  ,(0x4dcf,"HEXAGRAM FOR ENTHUSIASM")
  ,(0x4dd0,"HEXAGRAM FOR FOLLOWING")
  ,(0x4dd1,"HEXAGRAM FOR WORK ON THE DECAYED")
  ,(0x4dd2,"HEXAGRAM FOR APPROACH")
  ,(0x4dd3,"HEXAGRAM FOR CONTEMPLATION")
  ,(0x4dd4,"HEXAGRAM FOR BITING THROUGH")
  ,(0x4dd5,"HEXAGRAM FOR GRACE")
  ,(0x4dd6,"HEXAGRAM FOR SPLITTING APART")
  ,(0x4dd7,"HEXAGRAM FOR RETURN")
  ,(0x4dd8,"HEXAGRAM FOR INNOCENCE")
  ,(0x4dd9,"HEXAGRAM FOR GREAT TAMING")
  ,(0x4dda,"HEXAGRAM FOR MOUTH CORNERS")
  ,(0x4ddb,"HEXAGRAM FOR GREAT PREPONDERANCE")
  ,(0x4ddc,"HEXAGRAM FOR THE ABYSMAL WATER")
  ,(0x4ddd,"HEXAGRAM FOR THE CLINGING FIRE")
  ,(0x4dde,"HEXAGRAM FOR INFLUENCE")
  ,(0x4ddf,"HEXAGRAM FOR DURATION")
  ,(0x4de0,"HEXAGRAM FOR RETREAT")
  ,(0x4de1,"HEXAGRAM FOR GREAT POWER")
  ,(0x4de2,"HEXAGRAM FOR PROGRESS")
  ,(0x4de3,"HEXAGRAM FOR DARKENING OF THE LIGHT")
  ,(0x4de4,"HEXAGRAM FOR THE FAMILY")
  ,(0x4de5,"HEXAGRAM FOR OPPOSITION")
  ,(0x4de6,"HEXAGRAM FOR OBSTRUCTION")
  ,(0x4de7,"HEXAGRAM FOR DELIVERANCE")
  ,(0x4de8,"HEXAGRAM FOR DECREASE")
  ,(0x4de9,"HEXAGRAM FOR INCREASE")
  ,(0x4dea,"HEXAGRAM FOR BREAKTHROUGH")
  ,(0x4deb,"HEXAGRAM FOR COMING TO MEET")
  ,(0x4dec,"HEXAGRAM FOR GATHERING TOGETHER")
  ,(0x4ded,"HEXAGRAM FOR PUSHING UPWARD")
  ,(0x4dee,"HEXAGRAM FOR OPPRESSION")
  ,(0x4def,"HEXAGRAM FOR THE WELL")
  ,(0x4df0,"HEXAGRAM FOR REVOLUTION")
  ,(0x4df1,"HEXAGRAM FOR THE CAULDRON")
  ,(0x4df2,"HEXAGRAM FOR THE AROUSING THUNDER")
  ,(0x4df3,"HEXAGRAM FOR THE KEEPING STILL MOUNTAIN")
  ,(0x4df4,"HEXAGRAM FOR DEVELOPMENT")
  ,(0x4df5,"HEXAGRAM FOR THE MARRYING MAIDEN")
  ,(0x4df6,"HEXAGRAM FOR ABUNDANCE")
  ,(0x4df7,"HEXAGRAM FOR THE WANDERER")
  ,(0x4df8,"HEXAGRAM FOR THE GENTLE WIND")
  ,(0x4df9,"HEXAGRAM FOR THE JOYOUS LAKE")
  ,(0x4dfa,"HEXAGRAM FOR DISPERSION")
  ,(0x4dfb,"HEXAGRAM FOR LIMITATION")
  ,(0x4dfc,"HEXAGRAM FOR INNER TRUTH")
  ,(0x4dfd,"HEXAGRAM FOR SMALL PREPONDERANCE")
  ,(0x4dfe,"HEXAGRAM FOR AFTER COMPLETION")
  ,(0x4dff,"HEXAGRAM FOR BEFORE COMPLETION")]
