GL_GIT=git@gitlab.com:rd--/hmt.git
GL_HTTP=https://gitlab.com/rd--/hmt.git

all:
	echo "hmt"

mk-cmd:
	(cd cmd ; make all install)

mk-svg:
	(cd data/dot/tj/oh; sh mk.sh)

clean:
	rm -Rf dist dist-newstyle *~
	(cd cmd; make clean)

push-gl:
	git push $(GL_GIT)

pull-gl:
	git pull $(GL_HTTP)

push-tags:
	git push $(GL_GIT) --tags

update-rd:
	ssh rd@rohandrape.net "(cd sw/hmt; git pull $(GL_HTTP))"

push-all:
	make push-gl update-rd
