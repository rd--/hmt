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

data Pitch = Pitch { note :: Note_T
                   , alteration :: Alteration_T
                   , octave :: Octave }
           deriving (Eq, Show)

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

alteration_to_fdiff :: Alteration_T -> Double
alteration_to_fdiff a =
    case a of
      ThreeQuarterToneFlat -> -1.5
      QuarterToneFlat -> -0.5
      QuarterToneSharp -> 0.5
      ThreeQuarterToneSharp -> 1.5
      _ -> fromIntegral (alteration_to_diff a)

pitch_to_octpc :: Pitch -> (Octave, PitchClass)
pitch_to_octpc = midi_to_octpc . pitch_to_midi

pitch_to_midi :: Pitch -> Integer
pitch_to_midi (Pitch n a o) =
    let a' = alteration_to_diff a
        n' = note_to_pc n
    in 12 + o * 12 + n' + a'

pitch_to_fmidi :: Pitch -> Double
pitch_to_fmidi (Pitch n a o) =
    let a' = alteration_to_fdiff a
        o' = fromIntegral o
        n' = fromIntegral (note_to_pc n)
    in 12 + o' * 12 + n' + a'

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
    in Pitch n a o

octpc_nrm :: (Octave, PitchClass) -> (Octave, PitchClass)
octpc_nrm (o,pc) =
    if pc > 11
    then octpc_nrm (o+1,pc-12)
    else if pc < 0
         then octpc_nrm (o-1,pc+12)
         else (o,pc)

octpc_trs :: Integer -> (Octave, PitchClass) -> (Octave, PitchClass)
octpc_trs n (o,pc) = octpc_nrm (o,pc+n)

octpc_to_midi :: (Octave, PitchClass) -> Integer
octpc_to_midi (o,pc) = 60 + ((o - 4) * 12) + pc

midi_to_octpc :: Integer -> (Octave, PitchClass)
midi_to_octpc n = (n - 12) `divMod` 12

pitch_edit_octave :: (Integer -> Integer) -> Pitch -> Pitch
pitch_edit_octave f (Pitch n a o) = Pitch n a (f o)
