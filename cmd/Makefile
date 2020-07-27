cmd = csv-midi db gl obj pct ply scala
cmd_prefix = hmt

include ~/sw/hsc3/mk/cmd.mk

mk-gl:
	(make gl ; cp gl $(prefix)/bin/hmt-gl)

mk-gr-planar:
	(g++ -Wall gr-planar.cpp -o gr-planar ; mv gr-planar $(prefix)/bin/hmt-gr-planar)
