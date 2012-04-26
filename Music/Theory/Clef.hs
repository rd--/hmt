-- | Common music notation clefs.
module Music.Theory.Clef where

import Music.Theory.Pitch
import Music.Theory.Pitch.Name

-- | Clef enumeration type.
data Clef_T = Bass | Tenor | Alto | Treble | Percussion
              deriving (Eq,Ord,Show)

-- | Clef with octave offset.
data Integral i => Clef i = Clef {clef_t :: Clef_T
                                 ,clef_octave :: i}
                            deriving (Eq,Ord,Show)

-- | Give clef range as a 'Pitch' pair indicating the notes below and
-- above the staff.
--
-- > map clef_range [Treble,Bass] == [Just (d4,g5),Just (f2,b3)]
-- > clef_range Percussion == Nothing
clef_range :: Clef_T -> Maybe (Pitch,Pitch)
clef_range c =
    case c of
      Bass -> Just (f2,b3)
      Tenor -> Just (c3,f4)
      Alto -> Just (e3,a4)
      Treble -> Just (d4,g5)
      Percussion -> Nothing

-- | Suggest a 'Clef' given a 'Pitch'.
--
-- > map clef_suggest [c2,c4] == [Clef Bass (-1),Clef Treble 0]
clef_suggest :: Integral i => Pitch -> Clef i
clef_suggest p | p < f1 = Clef Bass (-2)
               | p < f2 = Clef Bass (-1)
               | p < b3 = Clef Bass 0
               | p < g5 = Clef Treble 0
               | p < g6 = Clef Treble 1
               | otherwise = Clef Treble 2
