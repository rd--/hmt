# pct

## ess = embedded segment search

~~~~
$ echo 23A | pct ess 0164325
2B013A9
923507A
$ echo 23A | hmt-pct ess 0164325
923507A
2B013A9
$
~~~~

## frg = fragmentation of cycles

~~~~
$ pct frg 024579
Fragmentation of 1-cycle(s):  [0-2-45-7-9--]
Fragmentation of 2-cycle(s):  [024---] [--579-]
Fragmentation of 3-cycle(s):  [0--9] [-47-] [25--]
Fragmentation of 4-cycle(s):  [04-] [-59] [2--] [-7-]
Fragmentation of 5-cycle(s):  [05------4927]
Fragmentation of 6-cycle(s):  [0-] [-7] [2-] [-9] [4-] [5-]
IC cycle vector: <1> <22> <111> <1100> <5> <000000>
$ hmt-pct frg 024579
Fragmentation of 1-cycle(s): [0-2-45-7-9--]
Fragmentation of 2-cycle(s): [024---] [--579-]
Fragmentation of 3-cycle(s): [0--9] [-47-] [25--]
Fragmentation of 4-cycle(s): [04-] [-59] [2--] [-7-]
Fragmentation of 5-cycle(s): [05------4927]
Fragmentation of 6-cycle(s): [0-] [-7] [2-] [-9] [4-] [5-]

IC cycle vector: <1> <22> <111> <1100> <5> <000000>
$
~~~~


## si = set information

~~~~
$ pct si 0586
pitch-class-set: {0568}
set-class: T5  4-Z29[0137]
interval-class-vector: [4111111]
tics: [222012202012]
complement: {123479AB} (T4I 8-Z29)
multiplication-by-five-transform: {0164} (T0  4-Z15)
$ echo 053B | hmt-pct si
pitch-class-set: {035B}
set-class: T11 4-Z15[0146]
interval-class-vector: [4111111]
tics: [221022221020]
complement: {1246789A} (T10I 8-Z15)
multiplication-by-five-transform: {0137} (T0 4-Z29)
$
~~~~

## trs = transformations search

~~~~
$ echo 642 | pct trs 024579 | sort -u
531642
6421B9
642753
B97642
$ echo 642 | hmt-pct trs 024579
6421B9
B97642
531642
642753
$
~~~~
