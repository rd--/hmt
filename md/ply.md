# ply

PLY data files

`ply-to-v3-graph` reads V3 vertex and edge data from a PLY file and writes a _LBL (V3 R) ()_ graph structure.

`v3-graph-to-ply` is the inverse.

~~~~
$ hmt-ply ply-to-v3-graph ~/rd/j/2020-04-27/ply/1AAR.A.ply > /tmp/x.hs
$ wc /tmp/x.hs
   1    1 5721 /tmp/x.hs
$ hmt-ply v3-graph-to-ply 2 /tmp/x.hs | head
ply
format ascii 1.0
element vertex 76
property float x
property float y
property float z
element edge 75
property int vertex1
property int vertex2
end_header
$
~~~~
