push-sp:
	darcs push -a rd@slavepianos.org:sw/hmt

pull-sp:
	darcs pull -a http://rd.slavepianos.org/sw/hmt

clean:
	cabal clean
	rm -Rf dist
	(cd cmd; make clean)
