all:
	echo "hmt"

push-rd:
	darcs push -a rd@rohandrape.net:sw/hmt

pull-rd:
	darcs pull -a http://rohandrape.net/sw/hmt

remote-update:
	ssh rd@rohandrape.net "(cd sw/hmt; make)"

clean:
	cabal clean
	rm -Rf dist
	(cd cmd; make clean)

mk-svg:
	(cd data/dot/tj/oh; sh mk.sh)
