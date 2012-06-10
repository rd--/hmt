-- | Common music notation dynamic marks.
module Music.Theory.Dynamic_Mark where

data Dynamic_Mark_T = Niente
                    | PPPPP | PPPP | PPP | PP | P | MP
                    | MF | F | FF | FFF | FFFF | FFFFF
                    | FP | SF | SFP | SFPP | SFZ | SFFZ
                      deriving (Eq,Ord,Enum,Bounded,Show)

data Hairpin_T = Crescendo | Diminuendo | End_Hairpin
                 deriving (Eq,Ord,Enum,Bounded,Show)

-- | The 'Hairpin_T' implied by a sequence of 'Dynamic_Mark_T's.
--
-- > map (implied_hairpin MF) [MP,F] == [Just Diminuendo,Just Crescendo]
implied_hairpin :: Dynamic_Mark_T -> Dynamic_Mark_T -> Maybe Hairpin_T
implied_hairpin p q =
    case compare p q of
      LT -> Just Crescendo
      EQ -> Nothing
      GT -> Just Diminuendo
