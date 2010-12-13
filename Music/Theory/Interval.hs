module Music.Theory.Interval where

import Data.List (unfoldr)
import Music.Theory.Pitch

data Interval_T = Unison | Second | Third | Fourth
                | Fifth | Sixth | Seventh
                  deriving (Eq, Ord, Enum, Show)

data Interval_Q = Diminished | Minor
                | Perfect
                | Major | Augmented
                  deriving (Eq, Ord, Enum, Show)

data Interval = Interval { interval_type :: Interval_T
                         , interval_quality :: Interval_Q
                         , interval_direction :: Ordering
                         , interval_octave :: Octave }
                deriving (Eq, Show)

interval_ty :: Note_T -> Note_T -> Interval_T
interval_ty n1 n2 = toEnum ((fromEnum n2 - fromEnum n1) `mod` 7)

interval_q_tbl :: [(Interval_T, [(Int, Interval_Q)])]
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

interval_q :: Interval_T -> Int -> Maybe Interval_Q
interval_q i n =
    case lookup i interval_q_tbl of
      Just t -> lookup n t
      Nothing -> Nothing

note_span :: Note_T -> Note_T -> [Note_T]
note_span n1 n2 =
    let fn x = toEnum (x `mod` 7)
        n1' = fromEnum n1
        n2' = fromEnum n2
        n2'' = if n1' > n2' then n2' + 7 else n2'
    in map fn [n1' .. n2'']

invert_ordering :: Ordering -> Ordering
invert_ordering x =
    case x of
      GT -> LT
      LT -> GT
      EQ -> EQ

interval :: Pitch -> Pitch -> Interval
interval p1 p2 =
    let c = pitch_cmp p1 p2
        (Pitch n1 _ _ o1) = p1
        (Pitch n2 _ _ o2) = p2
        p1' = pitch_to_pc p1
        p2' = pitch_to_pc p2
        st = (p2' - p1') `mod` 12
        ty = interval_ty n1 n2
        (Just qu) = interval_q ty (fromIntegral st)
        o_a = if n1 > n2 then -1 else 0
    in case c of
         GT -> (interval p2 p1) { interval_direction = GT }
         _ -> Interval ty qu c (o2 - o1 + o_a)

invert_interval :: Interval -> Interval
invert_interval (Interval t qu d o) =
    let d' = invert_ordering d
    in Interval t qu d' o

-- can this be written without knowing the Interval_T?
quality_difference :: Interval_Q -> Interval_Q -> Int
quality_difference a b =
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
        err = error ("quality_difference: " ++ show (a,b))
    in case fwd of
         Just n -> n
         Nothing -> case rvs of
                      Just n -> negate n
                      Nothing -> err

transpose :: Interval -> Pitch -> Pitch
transpose i ip =
    let (Pitch p_n p_a p_ar p_o) = ip
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
        ip' = Pitch p_n' p_a p_ar (p_o + i_o + oa)
        st = if i_d == GT
             then (pitch_to_pc ip - pitch_to_pc ip') `mod` 12
             else (pitch_to_pc ip' - pitch_to_pc ip) `mod` 12
        ty = if i_d == GT
             then interval_ty p_n' p_n
             else interval_ty p_n p_n'
        qu = maybe (error ("qu: " ++ show (ty,st))) id
             (interval_q ty (fromIntegral st))
        qd = quality_difference qu i_q * i_d'
        p_a' = toEnum (fromEnum p_a + (qd * 2))
    in ip' { alteration = p_a' }

circle_of_fifths :: ([Pitch], [Pitch])
circle_of_fifths =
    let c4 = Pitch C Natural Nothing 4
        p4 = Interval Fourth Perfect LT 0
        p5 = Interval Fifth Perfect LT 0
        mk y = unfoldr (\(x,i) -> if i == 12
                                  then Nothing
                                  else let x' = transpose y x
                                       in Just (x',(x',i+1))) (c4,0)
    in (mk p4,mk p5)
