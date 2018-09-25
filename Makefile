all:
	(cd data/dot/tj/oh; sh mk.sh)

push-sp:
	darcs push -a rd@slavepianos.org:sw/hmt

pull-sp:
	darcs pull -a http://rd.slavepianos.org/sw/hmt

remote-update:
	ssh rd@slavepianos.org "(cd sw/hmt; make)"

clean:
	cabal clean
	rm -Rf dist
	(cd cmd; make clean)
	rm -f data/dot/tj/oh/svg/*.svg
