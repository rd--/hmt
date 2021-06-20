-- | Common music notation intervals.
module Music.Theory.Interval where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.Ord as T {- hmt -}
import qualified Music.Theory.Pitch as T {- hmt -}
import qualified Music.Theory.Pitch.Note as T {- hmt -}

-- | Interval type or degree.
data Interval_T = Unison | Second | Third | Fourth
                | Fifth | Sixth | Seventh
                  deriving (Eq,Enum,Bounded,Ord,Show)

-- | Interval quality.
data Interval_Q = Diminished | Minor
                | Perfect
                | Major | Augmented
                  deriving (Eq,Enum,Bounded,Ord,Show)

-- | Common music notation interval.  An 'Ordering' of 'LT' indicates
-- an ascending interval, 'GT' a descending interval, and 'EQ' a
-- unison.
data Interval = Interval {interval_type :: Interval_T
                         ,interval_quality :: Interval_Q
                         ,interval_direction :: Ordering
                         ,interval_octave :: T.Octave}
                deriving (Eq,Show)

-- | Interval type between 'Note_T' values.
--
-- > map (interval_ty C) [E,B] == [Third,Seventh]
interval_ty :: T.Note_T -> T.Note_T -> Interval_T
interval_ty n1 n2 = toEnum ((fromEnum n2 - fromEnum n1) `mod` 7)

-- | Table of interval qualities.  For each 'Interval_T' gives
-- directed semitone interval counts for each allowable 'Interval_Q'.
-- For lookup function see 'interval_q', for reverse lookup see
-- 'interval_q_reverse'.
interval_q_tbl :: Integral n => [(Interval_T, [(n,Interval_Q)])]
interval_q_tbl =
    [(Unison,[(11,Diminished)
             ,(0,Perfect)
             ,(1,Augmented)])
    ,(Second,[(0,Diminished)
             ,(1,Minor)
             ,(2,Major)
             ,(3,Augmented)])
    ,(Third,[(2,Diminished)
            ,(3,Minor)
            ,(4,Major)
            ,(5,Augmented)])
    ,(Fourth,[(4,Diminished)
             ,(5,Perfect)
             ,(6,Augmented)])
    ,(Fifth,[(6,Diminished)
            ,(7,Perfect)
            ,(8,Augmented)])
    ,(Sixth,[(7,Diminished)
            ,(8,Minor)
            ,(9,Major)
            ,(10,Augmented)])
    ,(Seventh,[(9,Diminished)
              ,(10,Minor)
              ,(11,Major)
              ,(12,Augmented)])]

-- | Lookup 'Interval_Q' for given 'Interval_T' and semitone count.
--
-- > interval_q Unison 11 == Just Diminished
-- > interval_q Third 5 == Just Augmented
-- > interval_q Fourth 5 == Just Perfect
-- > interval_q Unison 3 == Nothing
interval_q :: Interval_T -> Int -> Maybe Interval_Q
interval_q i n = lookup i interval_q_tbl >>= lookup n

-- | Lookup semitone difference of 'Interval_T' with 'Interval_Q'.
--
-- > interval_q_reverse Third Minor == Just 3
-- > interval_q_reverse Unison Diminished == Just 11
interval_q_reverse :: Interval_T -> Interval_Q -> Maybe Int
interval_q_reverse ty qu =
    case lookup ty interval_q_tbl of
      Nothing -> Nothing
      Just tbl -> fmap fst (find ((== qu) . snd) tbl)

-- | Semitone difference of 'Interval'.
--
-- > interval_semitones (interval (Pitch C Sharp 4) (Pitch E Sharp 5)) == 16
-- > interval_semitones (interval (Pitch C Natural 4) (Pitch D Sharp 3)) == -9
interval_semitones :: Interval -> Int
interval_semitones (Interval ty qu dir oct) =
    case interval_q_reverse ty qu of
      Just n -> let o = 12 * oct
                in if dir == GT then negate n - o else n + o
      Nothing -> error "interval_semitones"

-- | Determine 'Interval' between two 'Pitch'es.
--
-- > interval (Pitch C Sharp 4) (Pitch D Flat 4) == Interval Second Diminished EQ 0
-- > interval (Pitch C Sharp 4) (Pitch E Sharp 5) == Interval Third Major LT 1
interval :: T.Pitch -> T.Pitch -> Interval
interval p1 p2 =
    let c = compare p1 p2
        (T.Pitch n1 _ o1) = p1
        (T.Pitch n2 _ o2) = p2
        p1' = T.pitch_to_pc p1
        p2' = T.pitch_to_pc p2
        st = (p2' - p1') `mod` 12
        ty = interval_ty n1 n2
        (Just qu) = interval_q ty (fromIntegral st)
        o_a = if n1 > n2 then -1 else 0
    in case c of
         GT -> (interval p2 p1) { interval_direction = GT }
         _ -> Interval ty qu c (o2 - o1 + o_a)

-- | Apply 'T.ord_invert' to 'interval_direction' of 'Interval'.
--
-- > invert_interval (Interval Third Major LT 1) == Interval Third Major GT 1
invert_interval :: Interval -> Interval
invert_interval (Interval t qu d o) = Interval t qu (T.ord_invert d) o

-- | The signed difference in semitones between two 'Interval_Q'
-- values when applied to the same 'Interval_T'.  Can this be written
-- correctly without knowing the Interval_T?
--
-- > quality_difference_m Minor Augmented == Just 2
-- > quality_difference_m Augmented Diminished == Just (-3)
-- > quality_difference_m Major Perfect == Nothing
quality_difference_m :: Interval_Q -> Interval_Q -> Maybe Int
quality_difference_m a b =
    let rule (x,y) =
            if x == y
            then Just 0
            else case (x,y) of
                   (Diminished,Minor) -> Just 1
                   (Diminished,Major) -> Just 2
                   (Diminished,Augmented) -> Just 3
                   (Minor,Major) -> Just 1
                   (Minor,Augmented) -> Just 2
                   (Major,Augmented) -> Just 1
                   (Diminished,Perfect) -> Just 1
                   (Perfect,Augmented) -> Just 1
                   _ -> Nothing
        fwd = rule (a,b)
        rvs = rule (b,a)
    in case fwd of
         Just n -> Just n
         Nothing -> case rvs of
                      Just n -> Just (negate n)
                      Nothing -> Nothing

-- | Erroring variant of 'quality_difference_m'.
quality_difference :: Interval_Q -> Interval_Q -> Int
quality_difference a b =
    let err = error ("quality_difference: " ++ show (a,b))
    in fromMaybe err (quality_difference_m a b)

-- | Transpose a 'Pitch' by an 'Interval'.
--
-- > transpose (Interval Third Diminished LT 0) (Pitch C Sharp 4) == Pitch E Flat 4
pitch_transpose :: Interval -> T.Pitch -> T.Pitch
pitch_transpose i ip =
    let (T.Pitch p_n p_a p_o) = ip
        (Interval i_t i_q i_d i_o) = i
        i_d' = if i_d == GT
               then -1
               else 1
        p_n' = toEnum ((fromEnum p_n + (fromEnum i_t * i_d')) `mod` 7)
        -- oa = octave alteration
        oa = if p_n' > p_n && i_d == GT
             then -1
             else if p_n' < p_n && i_d == LT
                  then 1
                  else 0
        ip' = T.Pitch p_n' p_a (p_o + i_o + oa)
        st = if i_d == GT
             then (T.pitch_to_pc ip - T.pitch_to_pc ip') `mod` 12
             else (T.pitch_to_pc ip' - T.pitch_to_pc ip) `mod` 12
        ty = if i_d == GT
             then interval_ty p_n' p_n
             else interval_ty p_n p_n'
        qu = let err = error ("qu: " ++ show (ty,st))
             in fromMaybe err (interval_q ty (fromIntegral st))
        qd = quality_difference qu i_q * i_d'
        p_a' = toEnum (fromEnum p_a + (qd * 2))
    in ip' {T.alteration = p_a'}

-- | Make leftwards (perfect fourth) and and rightwards (perfect
-- fifth) circles from 'Pitch'.
--
-- > let c = circle_of_fifths (Pitch F Sharp 4)
-- > in map pitch_to_pc (snd c) == [6,1,8,3,10,5,12,7,2,9,4,11]
circle_of_fifths :: T.Pitch -> ([T.Pitch], [T.Pitch])
circle_of_fifths x =
    let p4 = Interval Fourth Perfect LT 0
        p5 = Interval Fifth Perfect LT 0
        mk y = take 12 (iterate (pitch_transpose y) x)
    in (mk p4,mk p5)

-- | Parse a positive integer into interval type and octave
-- displacement.
--
-- > mapMaybe parse_interval_type (map show [1 .. 15])
parse_interval_type :: String -> Maybe (Interval_T,T.Octave)
parse_interval_type n =
    case reads n of
      [(n',[])] -> if n' == 0
                   then Nothing
                   else let (o,t) = (n' - 1) `divMod` 7
                        in Just (toEnum t,fromIntegral o)
      _ -> Nothing

-- | Parse interval quality notation.
--
-- > mapMaybe parse_interval_quality "dmPMA" == [minBound .. maxBound]
parse_interval_quality :: Char -> Maybe Interval_Q
parse_interval_quality q =
    let c = zip "dmPMA" [0..]
    in fmap toEnum (lookup q c)

-- | Degree of interval type and octave displacement.  Inverse of
-- 'parse_interval_type'.
--
-- > map interval_type_degree [(Third,0),(Second,1),(Unison,2)] == [3,9,15]
interval_type_degree :: (Interval_T,T.Octave) -> Int
interval_type_degree (t,o) = fromEnum t + 1 + (fromIntegral o * 7)

-- | Inverse of 'parse_interval_quality.
interval_quality_pp :: Interval_Q -> Char
interval_quality_pp q = "dmPMA" !! fromEnum q

-- | Parse standard common music interval notation.
--
-- > let i = mapMaybe parse_interval (words "P1 d2 m2 M2 A3 P8 +M9 -M2")
-- > in unwords (map interval_pp i) == "P1 d2 m2 M2 A3 P8 M9 -M2"
--
-- > mapMaybe (fmap interval_octave . parse_interval) (words "d1 d8 d15") == [-1,0,1]
parse_interval :: String -> Maybe Interval
parse_interval i =
    let unisons = [(Perfect,Unison)
                  ,(Diminished,Second)
                  ,(Augmented,Seventh)]
        f q n = case (parse_interval_quality q,parse_interval_type n) of
                    (Just q',Just (n',o)) ->
                       let o' = if (q',n') == (Diminished,Unison)
                                then o - 1
                                else o
                           d = if o' == 0 && (q',n') `elem` unisons
                               then EQ
                               else LT
                       in Just (Interval n' q' d o')
                    _ -> Nothing
    in case i of
         '-':q:n -> fmap invert_interval (f q n)
         '+':q:n -> f q n
         q:n -> f q n
         _ -> Nothing

-- | 'error' variant.
parse_interval_err :: String -> Interval
parse_interval_err = fromMaybe (error "parse_interval") . parse_interval

-- | Pretty printer for intervals, inverse of 'parse_interval'.
interval_pp :: Interval -> String
interval_pp (Interval n q d o) =
    let d' = if d == GT then ('-' :) else id
    in d' (interval_quality_pp q : show (interval_type_degree (n,o)))

-- | Standard names for the intervals within the octave, divided into
-- perfect, major and minor at the left, and diminished and augmented
-- at the right.
--
-- > let {bimap f (p,q) = (f p,f q)
-- >     ;f = mapMaybe (fmap interval_semitones . parse_interval)}
-- > in bimap f std_interval_names
std_interval_names :: ([String],[String])
std_interval_names =
    let pmM = "P1 m2 M2 m3 M3 P4 P5 m6 M6 m7 M7 P8"
        dA = "d2 A1 d3 A2 d4 A3 d5 A4 d6 A5 d7 A6 d8 A7"
    in (words pmM,words dA)
