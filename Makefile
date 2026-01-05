all:
	echo "hmt"

install:
	cabal v1-install --allow-newer

clean:
	rm -Rf dist dist-newstyle *~
	(cd cmd; make clean)

mk-cmd:
	(cd cmd ; make all install)

mk-svg:
	(cd data/dot/tj/oh; sh mk.sh)

push-all:
	r.gitlab-push.sh hmt
	r.github-push.sh hmt

indent:
	fourmolu -i Music

doctest:
	doctest -Wno-x-partial -Wno-incomplete-uni-patterns Music/Theory
