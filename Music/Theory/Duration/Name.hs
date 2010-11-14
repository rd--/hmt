module Music.Theory.Duration.Name where

import Music.Theory.Duration

-- * w,h,q,e,s

w,h,q,e,s :: Duration
w = whole_note
h = half_note
q = quarter_note
e = eighth_note
s = sixteenth_note

w',h',q',e',s' :: Duration
w' = dotted_whole_note
h' = dotted_half_note
q' = dotted_quarter_note
e' = dotted_eighth_note
s' = dotted_sixteenth_note

w'',h'',q'',e'',s'' :: Duration
w'' = Duration 1 2 1
h'' = Duration 2 2 1
q'' = Duration 4 2 1
e'' = Duration 8 2 1
s'' = Duration 16 2 1

-- * _1,_2,_4,_8,_16,_32

_1,_2,_4,_8,_16,_32 :: Duration
_1 = whole_note
_2 = half_note
_4 = quarter_note
_8 = eighth_note
_16 = sixteenth_note
_32 = Duration 32 0 1

_1',_2',_4',_8',_16',_32' :: Duration
_1' = dotted_whole_note
_2' = dotted_half_note
_4' = dotted_quarter_note
_8' = dotted_eighth_note
_16' = dotted_sixteenth_note
_32' = Duration 32 1 1

_1'',_2'',_4'',_8'',_16'',_32'' :: Duration
_1'' = Duration 1 2 1
_2'' = Duration 2 2 1
_4'' = Duration 4 2 1
_8'' = Duration 8 2 1
_16'' = Duration 16 2 1
_32'' = Duration 32 2 1
