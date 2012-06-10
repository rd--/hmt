-- | Common music notation dynamic marks.
module Music.Theory.Dynamic_Mark where

import Music.Theory.List

-- | Enumeration of dynamic mark symbols.
data Dynamic_Mark_T = Niente
                    | PPPPP | PPPP | PPP | PP | P | MP
                    | MF | F | FF | FFF | FFFF | FFFFF
                    | FP | SF | SFP | SFPP | SFZ | SFFZ
                      deriving (Eq,Ord,Enum,Bounded,Show)

-- | Enumeration of hairpin indicators.
data Hairpin_T = Crescendo | Diminuendo | End_Hairpin
                 deriving (Eq,Ord,Enum,Bounded,Show)

-- | The 'Hairpin_T' implied by a ordered pair of 'Dynamic_Mark_T's.
--
-- > map (implied_hairpin MF) [MP,F] == [Just Diminuendo,Just Crescendo]
implied_hairpin :: Dynamic_Mark_T -> Dynamic_Mark_T -> Maybe Hairpin_T
implied_hairpin p q =
    case compare p q of
      LT -> Just Crescendo
      EQ -> Nothing
      GT -> Just Diminuendo

-- | A node in a dynamic sequence.
type Dynamic_Node = (Maybe Dynamic_Mark_T,Maybe Hairpin_T)

-- | Calculate a 'Dynamic_Node' sequence from a sequence of
-- 'Dynamic_Mark_T's.
--
-- > dynamic_sequence [PP,MP,MP,PP] == [(Just PP,Just Crescendo)
-- >                                   ,(Just MP,Just End_Hairpin)
-- >                                   ,(Nothing,Just Diminuendo)
-- >                                   ,(Just PP,Just End_Hairpin)]
dynamic_sequence :: [Dynamic_Mark_T] -> [Dynamic_Node]
dynamic_sequence d =
    let h = zipWith implied_hairpin d (tail d) ++ [Nothing]
        e = Just End_Hairpin
        rec i p =
            case p of
              [] -> []
              [(j,_)] -> if i then [(j,e)] else [(j,Nothing)]
              (j,k):p' -> case k of
                            Nothing -> if i
                                       then (j,e) : rec False p'
                                       else (j,k) : rec False p'
                            Just _ -> (j,k) : rec True p'
    in rec False (zip (indicate_repetitions d) h)

-- | Apply 'Hairpin_T' and 'Dynamic_Mark_T' functions in that order as
-- required by 'Dynamic_Node'.
--
-- > let f _ x = show x
-- > in apply_dynamic_node f f (Nothing,Just Crescendo) undefined
apply_dynamic_node :: (a -> Dynamic_Mark_T -> a) -> (a -> Hairpin_T -> a)
                   -> Dynamic_Node -> a -> a
apply_dynamic_node f g (i,j) m =
    let n = maybe m (g m) j
    in maybe n (f n) i


