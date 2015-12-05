# scala

~~~~
$ hmt-scala env
SCALA_SCL_DIR = /home/rohan/data/scala/83/scl
$
~~~~

~~~~
$ hmt-scala db-stat
# entries        : 4557
# perfect-octave : 3911
# scale-uniform  : 2723
$
~~~~

~~~~
$ hmt-scala search ci 75 la monte young
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

## cps-tbl

Simple `CPS` table for tuning, indicating ET12 A=440 as reference.

~~~~
$ hmt-scala cps-tbl d12 young-lm_piano -74.7 3 60 72
~~~~

MNN PITCH    CPS   ET12    -/+
--- ----- ------ ------ ------
 60    C4 241.99 261.63 -19.64
 61   C#4 276.56 277.18  -0.63
 62    D4 272.23 293.66 -21.43
 63   Eb4 311.13 311.13  -0.00
 64    E4 316.06 329.63 -13.56
 65    F4 322.65 349.23 -26.58
 66   F#4 368.74 369.99  -1.25
 67    G4 362.98 392.00 -29.02
 68   Ab4 414.83 415.30  -0.47
 69    A4 421.42 440.00 -18.58
 70   Bb4 466.69 466.16   0.52
 71    B4 474.10 493.88 -19.79
 72    C5 483.97 523.25 -39.28
--- ----- ------ ------ ------
