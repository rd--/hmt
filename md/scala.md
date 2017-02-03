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
$ hmt-scala cps-tbl d12 young-lm_piano -74.7 3 60 72
~~~~

MNN PITCH    CPS NEAR   ET12    -/+
--- ----- ------ ---- ------ ------
 60    C4 241.99   B3 261.63 -19.64
 61   C#4 276.56  C#4 277.18  -0.63
 62    D4 272.23  C#4 293.66 -21.43
 63   Eb4 311.13  Eb4 311.13  -0.00
 64    E4 316.06  Eb4 329.63 -13.56
 65    F4 322.65   E4 349.23 -26.58
 66   F#4 368.74  F#4 369.99  -1.25
 67    G4 362.98  F#4 392.00 -29.02
 68   Ab4 414.83  Ab4 415.30  -0.47
 69    A4 421.42  Ab4 440.00 -18.58
 70   Bb4 466.69  Bb4 466.16   0.52
 71    B4 474.10  Bb4 493.88 -19.79
 72    C5 483.97   B4 523.25 -39.28
--- ----- ------ ---- ------ ------

~~~~
$ hmt-scala cps-tbl cps cet111 440 69 25 69 93
~~~~

MNN PITCH     CPS NEAR    ET12    -/+
--- ----- ------- ---- ------- ------
 69    A4  440.00   A4  440.00   0.00
 70   Bb4  469.26  Bb4  466.16   3.09
 71    B4  500.46   B4  493.88   6.58
 72    C5  533.74   C5  523.25  10.49
 73   C#5  569.23  C#5  554.37  14.86
 74    D5  607.08  Eb5  587.33  19.75
 75   Eb5  647.45   E5  622.25  25.19
 76    E5  690.50   F5  659.26  31.25
 77    F5  736.42  F#5  698.46  37.96
 78   F#5  785.38   G5  739.99  45.39
 79    G5  837.61  Ab5  783.99  53.62
 80   Ab5  893.30   A5  830.61  62.70
 81    A5  952.70  Bb5  880.00  72.70
 82   Bb5 1016.05   B5  932.33  83.73
 83    B5 1083.62  C#6  987.77  95.85
 84    C6 1155.67   D6 1046.50 109.17
 85   C#6 1232.52  Eb6 1108.73 123.79
 86    D6 1314.47   E6 1174.66 139.82
 87   Eb6 1401.88   F6 1244.51 157.37
 88    E6 1495.10  F#6 1318.51 176.59
 89    F6 1594.52   G6 1396.91 197.60
 90   F#6 1700.54  Ab6 1479.98 220.56
 91    G6 1813.62  Bb6 1567.98 245.64
 92   Ab6 1934.22   B6 1661.22 273.00
 93    A6 2062.83   C7 1760.00 302.83
--- ----- ------- ---- ------- ------

# fluidsynth

Generate tuning commands for the [fluidsynth](http://www.fluidsynth.org/) synthesiser.

~~~~
$ hmt-scala fluidsynth d12 young-lm_piano -74.7 3 "La Monte Young, The Well Tuned Piano" 0 0
tuning "La Monte Young, The Well Tuned Piano" 0 0
tune 0 0 0 0.00
tune 0 0 1 96.08
tune 0 0 2 68.82
tune 0 0 3 299.99
tune 0 0 4 327.26
tune 0 0 5 362.95
tune 0 0 6 594.13
tune 0 0 7 566.86
tune 0 0 8 798.04
tune 0 0 9 825.30
tune 0 0 10 1001.95
tune 0 0 11 1029.21
...
$
~~~~
