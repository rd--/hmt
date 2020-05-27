%.png : %.dot
	dot -T png $< > $@

%.svg : %.dot
	dot -T svg $< > $@

all: euler-j5-a.svg euler-j5-b.svg euler-j7.svg euler-wtp.svg

clean:
	rm -f *.svg
