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
