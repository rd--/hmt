# obj

OBJ data files

`obj-to-v3-graph` reads V3 vertex and edge data from a OBJ file and writes a _LBL (V3 R) ()_ graph structure.

`v3-graph-to-obj` is the inverse.

~~~~
$ hmt-obj obj-to-v3-graph ~/rd/j/2020-04-28/obj/1cag.obj > /tmp/x.hs
$ wc /tmp/x.hs
    1     1 27552 /tmp/x.hs
$ hmt-obj v3-graph-to-obj 2 /tmp/x.hs
v 3.43 3.10 7.95
v 4.72 3.66 8.50
v 5.89 2.84 7.97
v 5.80 1.60 7.94
...
$
~~~~
