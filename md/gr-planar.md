# gr-planar

This is a minor edit of the file
[straight_line_drawing.cpp](https://www.boost.org/doc/libs/1_73_0/libs/graph/example/straight_line_drawing.cpp)
by Aaron Windsor.

It reads a planar graph at `stdin` and writes the co-ordinates of a straight line drawing of the graph to `stdout`.

The planar graph (v,e) is given as the number of vertices, which are labelled from zero, followed by edges given as vertex pairs.

The number of input values is `1 + 2e`.

The example below is of the maximal planar graph on 7 vertices having 15 edges.

~~~~
$ hmt-gr-planar
7 0 1 1 2 2 3 3 0 3 4 4 5 5 6 6 3 0 4 1 3 3 5 2 6 1 4 1 5 1 6
^D
0 0 10 0 5 4 5 5 2 1 3 2 4 3
$
~~~~
