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

~~~~
$ hmt-scala cps-tbl d12 young-lm_piano -74.7 -3 60 72
~~~~

MNN PITCH    CPS NEAR   ET12 CENTS-/+
--- ----- ------ ---- ------ --------
 60    C4 260.74   C4 261.63     -5.9
 61   C#4 256.67   C4 277.18   -133.1
 62    D4 293.33   D4 293.66     -2.0
 63   Eb4 297.99   D4 311.13    -74.7
 64    E4 330.00   E4 329.63      1.9
 65    F4 335.24   E4 349.23    -70.8
 66   F#4 342.22   F4 369.99   -135.1
 67    G4 391.11   G4 392.00     -3.9
 68   Ab4 385.00   G4 415.30   -131.2
 69    A4 440.00   A4 440.00     -0.0
 70   Bb4 446.98   A4 466.16    -72.7
 71    B4 456.29  Bb4 493.88   -137.0
 72    C5 521.48   C5 523.25     -5.9
--- ----- ------ ---- ------ --------

~~~~
$ hmt-scala cps-tbl cps cet111 440 69 25 69 93
~~~~

MNN PITCH     CPS NEAR    ET12 CENTS-/+
--- ----- ------- ---- ------- --------
 69    A4  440.00   A4  440.00      0.0
 70   Bb4  469.26  Bb4  466.16     11.5
 71    B4  500.46   B4  493.88     22.9
 72    C5  533.74   C5  523.25     34.4
 73   C#5  569.23  C#5  554.37     45.8
 74    D5  607.08  Eb5  587.33     57.3
 75   Eb5  647.45   E5  622.25     68.7
 76    E5  690.50   F5  659.26     80.2
 77    F5  736.42  F#5  698.46     91.6
 78   F#5  785.38   G5  739.99    103.1
 79    G5  837.61  Ab5  783.99    114.5
 80   Ab5  893.30   A5  830.61    126.0
 81    A5  952.70  Bb5  880.00    137.4
 82   Bb5 1016.05   B5  932.33    148.9
 83    B5 1083.62  C#6  987.77    160.3
 84    C6 1155.67   D6 1046.50    171.8
 85   C#6 1232.52  Eb6 1108.73    183.2
 86    D6 1314.47   E6 1174.66    194.7
 87   Eb6 1401.88   F6 1244.51    206.1
 88    E6 1495.10  F#6 1318.51    217.6
 89    F6 1594.52   G6 1396.91    229.1
 90   F#6 1700.54  Ab6 1479.98    240.5
 91    G6 1813.62  Bb6 1567.98    252.0
 92   Ab6 1934.22   B6 1661.22    263.4
 93    A6 2062.83   C7 1760.00    274.9
--- ----- ------- ---- ------- --------

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
