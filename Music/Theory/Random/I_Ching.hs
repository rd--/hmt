-- | YIJING / I-CHING
module Music.Theory.Random.I_Ching where

import Control.Monad {- base -}
import Data.Maybe {- base -}
import Data.Int {- base -}
import System.Random {- random -}

import qualified Music.Theory.Bits as T {- hmt -}
import qualified Music.Theory.Read as T {- hmt -}
import qualified Music.Theory.Tuple as T {- hmt -}
import qualified Music.Theory.Unicode as T {- hmt -}

-- * LINE

-- | Line, indicated as sum.
data Line = L6 | L7 | L8 | L9 deriving (Eq,Show)

{-| (sum={6,7,8,9},
     (yarrow probablity={1,3,5,7}/16,
      three-coin probablity={2,6}/16,
      name,signification,symbol))
-}
type Line_Stat = (Line,(Rational,Rational,String,String,String))

-- | I-CHING chart as sequence of 4 'Line_Stat'.
i_ching_chart :: [Line_Stat]
i_ching_chart =
    [(L6,(1/16,2/16,"old yin","yin changing into yang","---x---"))
    ,(L7,(5/16,6/16,"young yang","yang unchanging","-------"))
    ,(L8,(7/16,6/16,"young yin","yin unchanging","--- ---"))
    ,(L9,(3/16,2/16,"old yang","yang changing into yin","---o---"))]

-- | Lines L6 and L7 are unbroken (since L6 is becoming L7).
line_unbroken :: Line -> Bool
line_unbroken n = n `elem` [L6,L7]

-- | If /b/ then L7 else L8.
line_from_bit :: Bool -> Line
line_from_bit b = if b then L7 else L8

-- | Seven character ASCII string for line.
line_ascii_pp :: Line -> String
line_ascii_pp n = fromMaybe (error "line_ascii_pp") (fmap T.p5_fifth (lookup n i_ching_chart))

-- | Is line (ie. sum) moving (ie. 6 or 9).
line_is_moving :: Line -> Bool
line_is_moving n = n `elem` [L6,L9]

-- | Old yin (L6) becomes yang (L7), and old yang (L9) becomes yin (L8).
line_complement :: Line -> Maybe Line
line_complement n =
    case n of
      L6 -> Just L7
      L9 -> Just L8
      _ -> Nothing

{- | Sequence of sum values assigned to ascending four bit numbers.
     Sequence is in ascending probablity, ie: 1×6,3×9,5×7,7×8.

> import Music.Theory.Bits {- hmt -}
> zip (map (gen_bitseq_pp 4) [0::Int .. 15]) (map line_ascii_pp four_coin_sequence)

-}
four_coin_sequence :: [Line]
four_coin_sequence =
    [L6,L9,L9,L9
    ,L7,L7,L7,L7
    ,L7,L8,L8,L8
    ,L8,L8,L8,L8]

-- * HEXAGRAM

-- | Sequence of 6 'Line'.
type Hexagram = [Line]

-- | Hexagrams are drawn upwards.
hexagram_pp :: Hexagram -> String
hexagram_pp = unlines . reverse . map line_ascii_pp

-- | Generate hexagram (ie. sequence of six lines given by sum) using 'four_coin_sequence'.
--
-- > four_coin_gen_hexagram >>= putStrLn . hexagram_pp
four_coin_gen_hexagram :: IO Hexagram
four_coin_gen_hexagram = fmap (map (four_coin_sequence !!)) (replicateM 6 (randomRIO (0,15)))

-- | 'any' of 'line_is_moving'.
hexagram_has_complement :: Hexagram -> Bool
hexagram_has_complement = any line_is_moving

-- | If 'hexagram_has_complement' then derive it.
--
-- > h <- four_coin_gen_hexagram
-- > putStrLn (hexagram_pp h)
-- > maybe (return ()) (putStrLn . hexagram_pp) (hexagram_complement h)
hexagram_complement :: Hexagram -> Maybe Hexagram
hexagram_complement h =
    let f n = fromMaybe n (line_complement n)
    in if hexagram_has_complement h then Just (map f h) else Nothing

-- | Names of hexagrams, in King Wen order (see also data/csv/combinatorics/yijing.csv)
--
-- > length hexagram_names == 64
hexagram_names :: [(String,String)]
hexagram_names =
    [("乾","qián")
    ,("坤","kūn")
    ,("屯","zhūn")
    ,("蒙","méng")
    ,("需","xū")
    ,("訟","sòng")
    ,("師","shī")
    ,("比","bǐ")
    ,("小畜","xiǎo chù")
    ,("履","lǚ")
    ,("泰","tài")
    ,("否","pǐ")
    ,("同人","tóng rén")
    ,("大有","dà yǒu")
    ,("謙","qiān")
    ,("豫","yù")
    ,("隨","suí")
    ,("蠱","gŭ")
    ,("臨","lín")
    ,("觀","guān")
    ,("噬嗑","shì kè")
    ,("賁","bì")
    ,("剝","bō")
    ,("復","fù")
    ,("無妄","wú wàng")
    ,("大畜","dà chù")
    ,("頤","yí")
    ,("大過","dà guò")
    ,("坎","kǎn")
    ,("離","lí")
    ,("咸","xián")
    ,("恆","héng")
    ,("遯","dùn")
    ,("大壯","dà zhuàng")
    ,("晉","jìn")
    ,("明夷","míng yí")
    ,("家人","jiā rén")
    ,("睽","kuí")
    ,("蹇","jiǎn")
    ,("解","xiè")
    ,("損","sǔn")
    ,("益","yì")
    ,("夬","guài")
    ,("姤","gòu")
    ,("萃","cuì")
    ,("升","shēng")
    ,("困","kùn")
    ,("井","jǐng")
    ,("革","gé")
    ,("鼎","dǐng")
    ,("震","zhèn")
    ,("艮","gèn")
    ,("漸","jiàn")
    ,("歸妹","guī mèi")
    ,("豐","fēng")
    ,("旅","lǚ")
    ,("巽","xùn")
    ,("兌","duì")
    ,("渙","huàn")
    ,("節","jié")
    ,("中孚","zhōng fú")
    ,("小過","xiǎo guò")
    ,("既濟","jì jì")
    ,("未濟","wèi jì")]

-- | Unicode hexagram characters, in King Wen order.
--
-- > import Data.List.Split {- split -}
-- > mapM_ putStrLn (chunksOf 8 hexagram_unicode_sequence)
hexagram_unicode_sequence :: [Char]
hexagram_unicode_sequence = map (toEnum . fst) T.yijing_tbl

-- | Binary form of 'Hexagram'.
hexagram_to_binary :: Hexagram -> Int8
hexagram_to_binary = T.pack_bitseq . map line_unbroken

-- | Show binary form.
hexagram_to_binary_str :: Hexagram -> String
hexagram_to_binary_str = T.gen_bitseq_pp 6 . hexagram_to_binary

-- | Inverse of 'hexagram_to_binary'.
hexagram_from_binary :: Int8 -> Hexagram
hexagram_from_binary = map line_from_bit . T.gen_bitseq 6

-- | Read binary form.
--
-- > let h = hexagram_from_binary_str "100010"
-- > putStrLn (hexagram_pp h)
-- > hexagram_to_binary_str h == "100010"
hexagram_from_binary_str :: String -> Hexagram
hexagram_from_binary_str = hexagram_from_binary . T.read_bin_err

-- * TRIGRAM

-- | Unicode sequence of trigrams (unicode order).
--
-- > import Data.List {- base -}
-- > putStrLn (intersperse ' ' trigram_unicode_sequence)
trigram_unicode_sequence :: [Char]
trigram_unicode_sequence = map (toEnum . fst) T.bagua_tbl

-- | (INDEX,UNICODE,BIT-SEQUENCE,NAME,NAME-TRANSLITERATION,NATURE-IMAGE,DIRECTION,ANIMAL)
--
-- > map (T.read_bin_err . T.p8_third) trigram_chart == [7,6,5,4,3,2,1,0]
trigram_chart :: [(Int, Char, String, Char, String, Char, String, Char)]
trigram_chart =
    [(1,'☰',"111",'乾',"qián",'天',"NW",'馬')
    ,(2,'☱',"110",'兌',"duì",'澤',"W",'羊')
    ,(3,'☲',"101",'離',"lí",'火',"S",'雉')
    ,(4,'☳',"100",'震',"zhèn",'雷',"E",'龍')
    ,(5,'☴',"011",'巽',"xùn",'風',"SE",'雞')
    ,(6,'☵',"010",'坎',"kǎn",'水',"N",'豕')
    ,(7,'☶',"001",'艮',"gèn",'山',"NE",'狗')
    ,(8,'☷',"000",'坤',"kūn",'地',"SW",'牛')]
