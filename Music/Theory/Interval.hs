-- | Common music notation intervals.
module Music.Theory.Interval where

import Data.Maybe
import Music.Theory.Pitch

-- | Interval type or degree.
data Interval_T = Unison | Second | Third | Fourth
                | Fifth | Sixth | Seventh
                  deriving (Eq,Enum,Bounded,Ord,Show)

-- | Interval quality.
data Interval_Q = Diminished | Minor
                | Perfect
                | Major | Augmented
                  deriving (Eq,Enum,Bounded,Ord,Show)

-- | Common music notation interval.
data Interval = Interval {interval_type :: Interval_T
                         ,interval_quality :: Interval_Q
                         ,interval_direction :: Ordering
                         ,interval_octave :: Octave}
                deriving (Eq,Show)

-- | Interval type between 'Note_T' values.
--
-- > map (interval_ty C) [E,B] == [Third,Seventh]
interval_ty :: Note_T -> Note_T -> Interval_T
interval_ty n1 n2 = toEnum ((fromEnum n2 - fromEnum n1) `mod` 7)

-- | Table of interval qualities.  For each 'Interval_T' gives
-- directed semitone interval counts for each allowable 'Interval_Q'.
-- For lookup function see 'interval_q'.
interval_q_tbl :: [(Interval_T, [(Int,Interval_Q)])]
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
interval_q :: Interval_T -> Int -> Maybe Interval_Q
interval_q i n =
    case lookup i interval_q_tbl of
      Just t -> lookup n t
      Nothing -> Nothing

-- | Inclusive set of 'Note_T' within indicated interval.  This is not
-- equal to 'enumFromTo' which is not circular.
--
-- > note_span E B == [E,F,G,A,B]
-- > note_span B D == [B,C,D]
-- > enumFromTo B D == []
note_span :: Note_T -> Note_T -> [Note_T]
note_span n1 n2 =
    let fn x = toEnum (x `mod` 7)
        n1' = fromEnum n1
        n2' = fromEnum n2
        n2'' = if n1' > n2' then n2' + 7 else n2'
    in map fn [n1' .. n2'']

-- | Invert 'Ordering', ie. 'GT' becomes 'LT' and vice versa.
--
-- > map invert_ordering [LT,EQ,GT] == [GT,EQ,LT]
invert_ordering :: Ordering -> Ordering
invert_ordering x =
    case x of
      GT -> LT
      LT -> GT
      EQ -> EQ

-- | Determine 'Interval' between two 'Pitch'es.
--
-- > interval (Pitch C Sharp 4) (Pitch D Flat 4) == Interval Second Diminished EQ 0
-- > interval (Pitch C Sharp 4) (Pitch E Sharp 5) == Interval Third Major LT 1
interval :: Pitch -> Pitch -> Interval
interval p1 p2 =
    let c = compare p1 p2
        (Pitch n1 _ o1) = p1
        (Pitch n2 _ o2) = p2
        p1' = pitch_to_pc p1
        p2' = pitch_to_pc p2
        st = (p2' - p1') `mod` 12
        ty = interval_ty n1 n2
        (Just qu) = interval_q ty (fromIntegral st)
        o_a = if n1 > n2 then -1 else 0
    in case c of
         GT -> (interval p2 p1) { interval_direction = GT }
         _ -> Interval ty qu c (o2 - o1 + o_a)

-- | Apply 'invert_ordering' to 'interval_direction' of 'Interval'.
--
-- > invert_interval (Interval Third Major LT 1) == Interval Third Major GT 1
invert_interval :: Interval -> Interval
invert_interval (Interval t qu d o) =
    let d' = invert_ordering d
    in Interval t qu d' o

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
    case quality_difference_m a b of
      Just n -> n
      Nothing -> error ("quality_difference: " ++ show (a,b))

-- | Transpose a 'Pitch' by an 'Interval'.
--
-- > transpose (Interval Third Diminished LT 0) (Pitch C Sharp 4) == Pitch E Flat 4
transpose :: Interval -> Pitch -> Pitch
transpose i ip =
    let (Pitch p_n p_a p_o) = ip
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
        ip' = Pitch p_n' p_a (p_o + i_o + oa)
        st = if i_d == GT
             then (pitch_to_pc ip - pitch_to_pc ip') `mod` 12
             else (pitch_to_pc ip' - pitch_to_pc ip) `mod` 12
        ty = if i_d == GT
             then interval_ty p_n' p_n
             else interval_ty p_n p_n'
        qu = let err = error ("qu: " ++ show (ty,st))
             in fromMaybe err (interval_q ty (fromIntegral st))
        qd = quality_difference qu i_q * i_d'
        p_a' = toEnum (fromEnum p_a + (qd * 2))
    in ip' { alteration = p_a' }

-- | Make leftwards (perfect fourth) and and rightwards (perfect
-- fifth) circles from 'Pitch'.
--
-- > let c = circle_of_fifths (Pitch F Sharp 4)
-- > in map pitch_to_pc (snd c) == [6,1,8,3,10,5,12,7,2,9,4,11]
circle_of_fifths :: Pitch -> ([Pitch], [Pitch])
circle_of_fifths x =
    let p4 = Interval Fourth Perfect LT 0
        p5 = Interval Fifth Perfect LT 0
        mk y = take 12 (iterate (transpose y) x)
    in (mk p4,mk p5)
