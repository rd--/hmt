all:
	echo "hmt"

mk-cmd:
	(cd cmd ; make all install)

mk-svg:
	(cd data/dot/tj/oh; sh mk.sh)

clean:
	rm -Rf dist dist-newstyle *~
	(cd cmd; make clean)

push-all:
	r.gitlab-push.sh hmt
