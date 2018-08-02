[env](#env),
[db-stat](#db-stat),
[search scale](#search-scale), [search mode](#search-mode),
[cps-table](#cps-tbl),
[intnam lookup](#intnam-lookup), [intnam search](#intnam-search)

# env

Print environment variable names, and values if set.

~~~~
$ hmt-scala env
SCALA_SCL_DIR = /home/rohan/data/scala/84/scl
SCALA_DIST_DIR = /home/rohan/opt/build/scala-22-pc64-linux
$
~~~~

# db-stat

~~~~
$ hmt-scala db-stat
# entries        : 4590
# perfect-octave : 3937
# scale-uniform  : 2761
$
~~~~

# db-summarise

~~~~
$ hmt-scala db-summarise 12 68 | grep convex | grep 7-limit
diaconv1029  : convex closure of 7-limit diamond with respect to 1029/1024
diaconv225   : convex closure of 7-limit diamond with respect to 225/224
diaconv2401  : convex closure of 7-limit diamond with respect to 2401/2400
diaconv3136  : convex closure of 7-limit diamond with respect to 3136/3125
diaconv4375  : convex closure of 7-limit diamond with respect to 4375/4374
diaconv5120  : convex closure of 7-limit diamond with respect to 5120/5103
diaconv6144  : convex closure of 7-limit diamond with respect to 6144/6125
diamond7_126 : 7-limit diamond starling (126/125) 5-limit convex closure
diamond7_225 : 7-limit diamond marvel (225/224) 5-limit convex closure
$
~~~~

# search scale

~~~~
$ hmt-scala search scale ci 75 la monte young
scale-name        : young-lm_guitar
scale-description : LaMonte Young, tuning of For Guitar '58. 1/1 March '92,
scale-degree      : 12
scale-type        : Pitch_Ratio
perfect-octave    : True
scale-cents-i     : [0,112,182,316,386,498,590,702,814,884,1018,1088,1200]
scale-ratios      : 1,16/15,10/9,6/5,5/4,4/3,45/32,3/2,8/5,5/3,9/5,15/8,2

scale-name        : young-lm_piano
scale-description : LaMonte Young's Well-Tuned Piano
scale-degree      : 12
scale-type        : Pitch_Ratio
perfect-octave    : True
scale-cents-i     : [0,177,204,240,444,471,675,702,738,942,969,1173,1200]
scale-ratios      : 1,567/512,9/8,147/128,1323/1024,21/16,189/128,3/2,49/32

scale-name        : young-lm_piano_1964
scale-description : LaMonte Young's Well-Tuned Piano (1964)
scale-degree      : 12
scale-type        : Pitch_Ratio
perfect-octave    : True
scale-cents-i     : [0,149,204,240,471,647,675,702,738,969,1145,1173,1200]
scale-ratios      : 1,279/256,9/8,147/128,21/16,93/64,189/128,3/2,49/32,7/4
$
~~~~

# search mode

~~~~
$ hmt-scala search mode ci nil xenakis
mode-start-degree : 0
mode-intervals    : 5,19,6,12,5,19,6
mode-degree       : 72
mode-description  : Xenakis Byzantine Liturgical Chromatic

mode-start-degree : 0
mode-intervals    : 7,16,7,12,7,16,7
mode-degree       : 72
mode-description  : Xenakis Byzantine Liturgical Soft Chromatic

mode-start-degree : 0
mode-intervals    : 12,11,7,12,12,11,7
mode-degree       : 72
mode-description  : Xenakis Byzantine Liturgical Diatonic, Misaelides 4th plagal Byzantine
$
~~~~

## cps-tbl

Simple `CPS` table for tuning, indicating ET12 A=440 as reference.
Table format can be `csv` or `md`.
The table is printed for indicated midi note number range (A0 = 21, C8 = 108).

~~~~
$ hmt-scala cps-tbl md d12 young-lm_piano -74.7 -3 60 72
~~~~

MNN    CPS ET12 CENTS-/+ REF CPS REF ET12 CENTS-/+
--- ------ ---- -------- ------- -------- --------
 60 260.74   C4     -5.9  261.63       C4     -5.9
 61 256.67   C4    -33.1  277.18      C#4   -133.1
 62 293.33   D4     -2.0  293.66       D4     -2.0
 63 297.99   D4     25.3  311.13      Eb4    -74.7
 64 330.00   E4      1.9  329.63       E4      1.9
 65 335.24   E4     29.2  349.23       F4    -70.8
 66 342.22   F4    -35.1  369.99      F#4   -135.1
 67 391.11   G4     -3.9  392.00       G4     -3.9
 68 385.00   G4    -31.2  415.30      Ab4   -131.2
 69 440.00   A4     -0.0  440.00       A4     -0.0
 70 446.98   A4     27.3  466.16      Bb4    -72.7
 71 456.29  Bb4    -37.0  493.88       B4   -137.0
 72 521.48   C5     -5.9  523.25       C5     -5.9
--- ------ ---- -------- ------- -------- --------

~~~~
$ hmt-scala cps-tbl md cps cet111 440 69 25 69 93
~~~~

MNN     CPS ET12 CENTS-/+ REF CPS REF ET12 CENTS-/+
--- ------- ---- -------- ------- -------- --------
 69 1308.76   E6    -12.8  440.00       A4   1887.2
 70 1395.79   F6     -1.4  466.16      Bb4   1898.6
 71 1488.60  F#6     10.1  493.88       B4   1910.1
 72 1587.59   G6     21.5  523.25       C5   1921.5
 73 1693.15  Ab6     33.0  554.37      C#5   1933.0
 74 1805.74   A6     44.4  587.33       D5   1944.4
 75 1925.81   B6    -44.1  622.25      Eb5   1955.9
 76 2053.87   C7    -32.7  659.26       E5   1967.3
 77 2190.44  C#7    -21.2  698.46       F5   1978.8
 78 2336.09   D7     -9.8  739.99      F#5   1990.2
 79 2491.43  Eb7      1.7  783.99       G5   2001.7
 80 2657.10   E7     13.1  830.61      Ab5   2013.1
 81 2833.78   F7     24.6  880.00       A5   2024.6
 82 3022.21  F#7     36.0  932.33      Bb5   2036.0
 83 3223.18   G7     47.5  987.77       B5   2047.5
 84 3437.50   A7    -41.1 1046.50       C6   2058.9
 85 3666.08  Bb7    -29.6 1108.73      C#6   2070.4
 86 3909.85   B7    -18.2 1174.66       D6   2081.8
 87 4169.84   C8     -6.7 1244.51      Eb6   2093.3
 88 4447.11  C#8      4.8 1318.51       E6   2104.8
 89 4742.82   D8     16.2 1396.91       F6   2116.2
 90 5058.19  Eb8     27.7 1479.98      F#6   2127.7
 91 5394.54   E8     39.1 1567.98       G6   2139.1
 92 5753.25   F8     50.6 1661.22      Ab6   2150.6
 93 6135.81   G8    -38.0 1760.00       A6   2162.0
 94 6543.81  Ab8    -26.5 1864.66      Bb6   2173.5
--- ------- ---- -------- ------- -------- --------

# fluidsynth

Generate tuning commands for the [fluidsynth](http://www.fluidsynth.org/) synthesiser.

~~~~
$ hmt-scala fluidsynth d12 young-lm_piano -74.7 -3 "La Monte Young, The Well Tuned Piano" 0 0
tuning "La Monte Young, The Well Tuned Piano" 0 0
tune 0 0 0 0.00
tune 0 0 1 0.00
tune 0 0 2 198.04
tune 0 0 3 225.30
tune 0 0 4 401.95
tune 0 0 5 429.21
tune 0 0 6 464.91
tune 0 0 7 696.08
tune 0 0 8 668.82
tune 0 0 9 899.99
tune 0 0 10 927.26
tune 0 0 11 962.95
...
$
~~~~

# intnam lookup

Lookup name of interval given by ratio, print also the cents value of the interval.

~~~~
$ hmt-scala intnam lookup 7/4 7/6 9/8 13/8 21/16 35/32 16/9
7:4 = harmonic seventh = 969
7:6 = septimal minor third = 267
9:8 = major whole tone = 204
13:8 = tridecimal neutral sixth = 841
21:16 = narrow fourth = 471
35:32 = septimal neutral second = 155
16:9 = Pythagorean minor seventh = 996
$ hmt-scala intnam lookup 256/243 264/256 288/264 294/288 324/294 4/3
256:243 = limma, Pythagorean minor second = 90
33:32 = undecimal comma, al-Farabi's 1/4-tone = 53
12:11 = 3/4-tone, undecimal neutral second = 151
49:48 = slendro diesis, septimal 1/6-tone = 36
54:49 = Zalzal's mujannab = 168
4:3 = perfect fourth = 498
$
~~~~

# intnam search

Lookup intervals with names that, case insenstively, include the indicated text.

~~~~
$ hmt-scala intnam search didymus
81:80 = syntonic comma, Didymus comma = 22
$
~~~~
