-- | <http://en.wikipedia.org/wiki/Braille_Patterns>
module Music.Theory.Braille where

import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Text.Printf {- base -}

-- | Braille coding data.  Elements are: (ASCII HEX,ASCII CHAR,DOT
-- LIST,UNICODE CHAR,MEANING).  The dot numbers are in column order.
type BRAILLE = (Int,Char,[Int],Char,String)

-- | Unicode 'Char' of 'BRAILLE'.
braille_unicode :: BRAILLE -> Char
braille_unicode (_,_,_,c',_) = c'

-- | Dot list of 'BRAILLE'.
braille_dots :: BRAILLE -> [Int]
braille_dots (_,_,d,_,_) = d

-- | ASCII Braille table.
--
-- > all id (map (\(x,c,_,_,_) -> x == fromEnum c) braille_table) == True
braille_table :: [BRAILLE]
braille_table =
    [(0x20,' ',[],'⠀'," ")
    ,(0x21,'!',[2,3,4,6],'⠮',"the")
    ,(0x22,'"',[5],'⠐',"contraction")
    ,(0x23,'#',[3,4,5,6],'⠼',"number prefix")
    ,(0x24,'$',[1,2,4,6],'⠫',"ed")
    ,(0x25,'%',[1,4,6],'⠩',"sh")
    ,(0x26,'&',[1,2,3,4,6],'⠯',"and")
    ,(0x27,'\'',[3],'⠄',"'")
    ,(0x28,'(',[1,2,3,5,6],'⠷',"of")
    ,(0x29,')',[2,3,4,5,6],'⠾',"with")
    ,(0x2A,'*',[1,6],'⠡',"ch")
    ,(0x2B,'+',[3,4,6],'⠬',"ing")
    ,(0x2C,',',[6],'⠠',"uppercase prefix")
    ,(0x2D,'-',[3,6],'⠤',"-")
    ,(0x2E,'.',[4,6],'⠨',"italic prefix")
    ,(0x2F,'/',[3,4],'⠌',"st")
    ,(0x30,'0',[3,5,6],'⠴',"”")
    ,(0x31,'1',[2],'⠂',",")
    ,(0x32,'2',[2,3],'⠆',";")
    ,(0x33,'3',[2,5],'⠒',":")
    ,(0x34,'4',[2,5,6],'⠲',".")
    ,(0x35,'5',[2,6],'⠢',"en")
    ,(0x36,'6',[2,3,5],'⠖',"!")
    ,(0x37,'7',[2,3,5,6],'⠶',"( or )")
    ,(0x38,'8',[2,3,6],'⠦',"“ or ?")
    ,(0x39,'9',[3,5],'⠔',"in")
    ,(0x3A,':',[1,5,6],'⠱',"wh")
    ,(0x3B,';',[5,6],'⠰',"letter prefix")
    ,(0x3C,'<',[1,2,6],'⠣',"gh")
    ,(0x3D,'=',[1,2,3,4,5,6],'⠿',"for")
    ,(0x3E,'>',[3,4,5],'⠜',"ar")
    ,(0x3F,'?',[1,4,5,6],'⠹',"th")
    ,(0x40,'@',[4],'⠈',"accent prefix")
    ,(0x41,'A',[1],'⠁',"a")
    ,(0x42,'B',[1,2],'⠃',"b")
    ,(0x43,'C',[1,4],'⠉',"c")
    ,(0x44,'D',[1,4,5],'⠙',"d")
    ,(0x45,'E',[1,5],'⠑',"e")
    ,(0x46,'F',[1,2,4],'⠋',"f")
    ,(0x47,'G',[1,2,4,5],'⠛',"g")
    ,(0x48,'H',[1,2,5],'⠓',"h")
    ,(0x49,'I',[2,4],'⠊',"i")
    ,(0x4A,'J',[2,4,5],'⠚',"j")
    ,(0x4B,'K',[1,3],'⠅',"k")
    ,(0x4C,'L',[1,2,3],'⠇',"l")
    ,(0x4D,'M',[1,3,4],'⠍',"m")
    ,(0x4E,'N',[1,3,4,5],'⠝',"n")
    ,(0x4F,'O',[1,3,5],'⠕',"o")
    ,(0x50,'P',[1,2,3,4],'⠏',"p")
    ,(0x51,'Q',[1,2,3,4,5],'⠟',"q")
    ,(0x52,'R',[1,2,3,5],'⠗',"r")
    ,(0x53,'S',[2,3,4],'⠎',"s")
    ,(0x54,'T',[2,3,4,5],'⠞',"t")
    ,(0x55,'U',[1,3,6],'⠥',"u")
    ,(0x56,'V',[1,2,3,6],'⠧',"v")
    ,(0x57,'W',[2,4,5,6],'⠺',"w")
    ,(0x58,'X',[1,3,4,6],'⠭',"x")
    ,(0x59,'Y',[1,3,4,5,6],'⠽',"y")
    ,(0x5A,'Z',[1,3,5,6],'⠵',"z")
    ,(0x5B,'[',[2,4,6],'⠪',"ow")
    ,(0x5C,'\\',[1,2,5,6],'⠳',"ou")
    ,(0x5D,']',[1,2,4,5,6],'⠻',"er")
    ,(0x5E,'^',[4,5],'⠘',"currency prefix")
    ,(0x5F,'_',[4,5,6],'⠸',"contraction")
    ]

-- | Lookup 'BRAILLE' value for unicode character.
--
-- > braille_lookup_unicode '⠝' == Just (0x4E,'N',[1,3,4,5],'⠝',"n")
braille_lookup_unicode :: Char -> Maybe BRAILLE
braille_lookup_unicode c =
    let f (_,_,_,c',_) = c == c'
    in find f braille_table

-- | Lookup 'BRAILLE' value for ascii character (case invariant).
--
-- > braille_lookup_ascii 'N' == Just (0x4E,'N',[1,3,4,5],'⠝',"n")
braille_lookup_ascii :: Char -> Maybe BRAILLE
braille_lookup_ascii c =
    let f (_,c',_,_,_) = toUpper c == c'
    in find f braille_table

-- | The arrangement of the 6-dot patterns into /decades/, sequences
-- of (1,10,3) cells.  The cell to the left of the decade is the empty
-- cell, the two cells to the right are the first two cells of the
-- decade shifted right.
--
-- For each decade there are two extra cells that shift
-- the first two cells of the decade right one place.  Subsequent
-- decades are derived by simple transformation of the first.  The
-- second is the first with the addition of dot @3@, the third adds
-- dots @3@ and @6@, the fourth adds dot @6@ and the fifth shifts the
-- first down one row.
--
-- The first decade has the 13 of the 16 4-dot patterns, the remaining
-- 3 are in the fifth decade, that is they are the three 4-dot
-- patterns that are down shifts of a 4-dot pattern.
--
-- > let trimap f (p,q,r) = (f p,f q,f r)
-- > let f = map (fromJust . decode) in map (trimap f) braille_64
braille_64 :: [(String,String,String)]
braille_64 =
    [("⠀","⠁⠃⠉⠙⠑⠋⠛⠓⠊⠚","⠈⠘")
    ,("⠄","⠅⠇⠍⠝⠕⠏⠟⠗⠎⠞","⠌⠜")
    ,("⠤","⠥⠧⠭⠽⠵⠯⠿⠷⠮⠾","⠬⠼")
    ,("⠠","⠡⠣⠩⠹⠱⠫⠻⠳⠪⠺","⠨⠸")
    ,("","⠂⠆⠒⠲⠢⠖⠶⠦⠔⠴","⠐⠰")]

-- | Transcribe ASCII to unicode braille.
--
-- > transcribe_unicode "ROHAN DRAPE" == "⠗⠕⠓⠁⠝⠀⠙⠗⠁⠏⠑"
transcribe_unicode :: String -> String
transcribe_unicode = map (braille_unicode . fromJust . braille_lookup_ascii)

-- > let ch = (white_circle,black_circle)
-- > let ch = (' ','.')
-- > putStrLn$ transcribe_char_grid ch "ROHAN DRAPE"
-- > putStrLn$ string_html_table $ transcribe_char_grid ch "ERKKI VELTHEIM"
transcribe_char_grid :: (Char,Char) -> String -> String
transcribe_char_grid (w,b) =
    unlines .
    map concat .
    transpose .
    map (dots_grid (w,b) . braille_dots . fromJust . braille_lookup_ascii)

-- | Generate 6-dot grid given (white,black) values.
--
-- > dots_grid (0,1) [1,2,3,5] == [[1,0],[1,1],[1,0]]
dots_grid :: (c,c) -> [Int] -> [[c]]
dots_grid (w,b) d =
    let f n = if n `elem` d then b else w
    in map (map f) [[1,4],[2,5],[3,6]]

string_html_table :: String -> String
string_html_table s =
    let f x = "<td>" ++ [x] ++ "</td>"
        g x = "<tr>" ++ concatMap f x ++ "</tr>"
        h x = "<table>" ++ concatMap g x ++ "</table>"
    in h (lines s)

-- | Decoding.
--
-- > let t0 = ["⠠⠁⠇⠇⠀⠓⠥⠍⠁⠝⠀⠆⠬⠎⠀⠜⠑⠀⠃⠕⠗⠝⠀⠋⠗⠑⠑⠀⠯⠀⠑⠟⠥⠁⠇⠀⠔⠀⠙⠊⠛⠝⠰⠽⠀⠯⠀⠐⠗⠎⠲"
-- >          ,"⠠⠮⠽⠀⠜⠑⠀⠢⠙⠪⠫⠀⠾⠀⠗⠂⠎⠕⠝⠀⠯⠀⠒⠎⠉⠊⠰⠑⠀⠯⠀⠩⠙⠀⠁⠉⠞⠀⠞⠪⠜⠙⠎⠀⠐⠕⠀⠁⠝⠕⠤"
-- >          ,"⠮⠗⠀⠔⠀⠁⠀⠸⠎⠀⠷⠀⠃⠗⠕⠮⠗⠓⠕⠕⠙⠲"]
--
-- > concatMap (fromMaybe "#" . decode) (concat t0)
decode :: Char -> Maybe String
decode c =
    case braille_lookup_unicode c of
      Just (_,_,_,_,s) -> Just s
      Nothing -> Nothing

-- | Start and end unicode indices.
braille_rng :: Integral i => (i,i)
braille_rng = (0x2800,0x28FF)

-- | All characters, in sequence.
--
-- > length braille_seq == 256
-- > putStrLn braille_seq
braille_seq :: [Char]
braille_seq = let (l,r) = braille_rng in [toEnum l .. toEnum r]

-- | The /n/th character, zero indexed.
braille_char :: Int -> Char
braille_char = toEnum . (+) 0x2800

-- | Two element index, 255 * 255 = 65025 places.
--
-- > map braille_ix [100,300]
braille_ix :: Int -> (Char,Char)
braille_ix n =
    let (i,j) = n `divMod` 255
        f k = braille_char (k + 1)
    in (f i,f j)

-- | HTML character encoding (as hex integer).
--
-- > unwords $ map unicode_html braille_seq
unicode_html :: Char -> String
unicode_html = printf "&#x%x;" . fromEnum

-- * Unicode

-- | White (empty) circle.
white_circle :: Char
white_circle = '○'

-- | Black (filled) circle.
black_circle :: Char
black_circle = '●'

-- | Shaded (hatched) circle.
shaded_circle :: Char
shaded_circle = '◍'

-- * Contractions

-- | Table of one letter contractions.
one_letter_contractions :: [(Char,String)]
one_letter_contractions =
    [('⠃',"but")
    ,('⠉',"can")
    ,('⠙',"do")
    ,('⠑',"every")
    ,('⠋',"from,-self")
    ,('⠛',"go")
    ,('⠓',"have")
    ,('⠚',"just")
    ,('⠅',"knowledge")
    ,('⠇',"like")
    ,('⠍',"more")
    ,('⠝',"not")
    ,('⠏',"people")
    ,('⠟',"quite")
    ,('⠗',"rather")
    ,('⠎',"so")
    ,('⠞',"that")
    ,('⠌',"still")
    ,('⠥',"us")
    ,('⠧',"very")
    ,('⠭',"it")
    ,('⠽',"you")
    ,('⠵',"as")
    ,('⠡',"child")
    ,('⠩',"shall")
    ,('⠹',"this")
    ,('⠱',"which")
    ,('⠳',"out")
    ,('⠺',"will")
    ,('⠆',"be,be-")
    ,('⠒',"con-")
    ,('⠲',"dis-")
    ,('⠢',"enough")
    ,('⠖',"to")
    ,('⠶',"were")
    ,('⠦',"his")
    ,('⠔',"in")
    ,('⠴',"by,was")
    ,('⠤',"com-")
    ]
