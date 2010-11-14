module Music.Theory.Pitch where

import Data.Function

type PitchClass = Integer
type Octave = Integer

data Note_T = C | D | E | F | G | A | B
              deriving (Eq, Ord, Enum, Show)

data Alteration_T = DoubleFlat
                  | ThreeQuarterToneFlat | Flat | QuarterToneFlat
                  | Natural
                  | QuarterToneSharp | Sharp | ThreeQuarterToneSharp
                  | DoubleSharp
                    deriving (Eq, Ord, Enum, Show)

data Alteration_Rule = ReminderAccidental
                     | CautionaryAccidental
                       deriving (Eq, Ord, Show)

data Pitch = Pitch { note :: Note_T
                   , alteration :: Alteration_T
                   , alteration_rule :: (Maybe Alteration_Rule)
                   , octave :: Octave }
           deriving (Eq, Show)

-- | Set alteration rule at pitch.
set_alt_rule :: Alteration_Rule -> Pitch -> Pitch
set_alt_rule x (Pitch n a _ o) = Pitch n a (Just x) o

note_to_pc :: Note_T -> Integer
note_to_pc n =
    case n of
      C -> 0
      D -> 2
      E -> 4
      F -> 5
      G -> 7
      A -> 9
      B -> 11

alteration_to_diff :: Alteration_T -> Integer
alteration_to_diff a =
    case a of
      DoubleFlat -> -2
      Flat -> -1
      Natural -> 0
      Sharp -> 1
      DoubleSharp -> 2
      _ -> error "alteration_to_diff: quarter tone"

pitch_to_octpc :: Pitch -> (Octave, PitchClass)
pitch_to_octpc (Pitch n a _ o) =
    let a' = alteration_to_diff a
    in (o, (note_to_pc n + a') `mod` 12)

pitch_to_pc :: Pitch -> PitchClass
pitch_to_pc = snd . pitch_to_octpc

pitch_cmp :: Pitch -> Pitch -> Ordering
pitch_cmp = compare `on` pitch_to_octpc

octpc_to_pitch :: (Octave, PitchClass) -> Pitch
octpc_to_pitch (o,pc) =
    let (n,a) = case pc of
                  0 -> (C,Natural)
                  1 -> (C,Sharp)
                  2 -> (D,Natural)
                  3 -> (E,Flat)
                  4 -> (E,Natural)
                  5 -> (F,Natural)
                  6 -> (F,Sharp)
                  7 -> (G,Natural)
                  8 -> (A,Flat)
                  9 -> (A,Natural)
                  10 -> (B,Flat)
                  11 -> (B,Natural)
                  _ -> error ("octpc_to_pitch: " ++ show pc)
    in Pitch n a Nothing o
