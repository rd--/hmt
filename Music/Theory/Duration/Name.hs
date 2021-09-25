-- | Names for common music notation durations.
module Music.Theory.Duration.Name where

import Music.Theory.Duration {- hmt -}

-- * Constants

breve,whole_note,half_note,quarter_note,eighth_note,sixteenth_note,thirtysecond_note :: Duration
breve = Duration 0 0 1
whole_note = Duration 1 0 1
half_note = Duration 2 0 1
quarter_note = Duration 4 0 1
eighth_note = Duration 8 0 1
sixteenth_note = Duration 16 0 1
thirtysecond_note = Duration 32 0 1

dotted_breve,dotted_whole_note,dotted_half_note,dotted_quarter_note,dotted_eighth_note,dotted_sixteenth_note,dotted_thirtysecond_note :: Duration
dotted_breve = Duration 0 1 1
dotted_whole_note = Duration 1 1 1
dotted_half_note = Duration 2 1 1
dotted_quarter_note = Duration 4 1 1
dotted_eighth_note = Duration 8 1 1
dotted_sixteenth_note = Duration 16 1 1
dotted_thirtysecond_note = Duration 32 1 1

double_dotted_breve,double_dotted_whole_note,double_dotted_half_note,double_dotted_quarter_note,double_dotted_eighth_note,double_dotted_sixteenth_note,double_dotted_thirtysecond_note :: Duration
double_dotted_breve = Duration 0 2 1
double_dotted_whole_note = Duration 2 2 1
double_dotted_half_note = Duration 2 2 1
double_dotted_quarter_note = Duration 4 2 1
double_dotted_eighth_note = Duration 8 2 1
double_dotted_sixteenth_note = Duration 16 2 1
double_dotted_thirtysecond_note = Duration 32 2 1
