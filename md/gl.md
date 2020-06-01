# gl

`obj-gr` loads a graph (a set of vertices and a set of edges) from one
or more `OBJ` or `OFF` or `4OFF` files and displays it.

If `--chain` is set input OBJ files that are vertex lists only will be
loaded as chains (ie. lines will be drawn between each successive
vertex).

If `--normalise` is set vertex data is normalised to (-1,1) after all
input data is loaded.

Key commands are:

- ROTATE: Y = `←`/`→` ; X = `↓`|`↑` ; Z = `PgUp`/`PgDn`
- TRANSLATE: X = `Ctl` `←`/`→` ; Y = `Ctl` `↓`|`↑` ; Z= `Ctl` `PgUp`/`PgDn`
- ZOOM OUT/IN: MINOR = `-`/`=` ; MAJOR = `Ctl` `-`/`=`
- ROTATE: (| `Alt`)
  `1`=(0,0|180,0)
  `2`=(90|270,0,0)
  `3`=(90|270,0,90|270)
  `4`=(0,0,90|270)
  `5`=(90,0|180,180|0)
- R4->R3 PROJECTION:
  `F1`=XYZ
  `F2`=XYW
  `F3`=XZW
  `F4`=YZW
- INITIAL STATE: `0`
- ON-SCREEN-DISPLAY: `o`
- QUIT: `Q`
