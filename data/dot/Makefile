%.png : %.dot
	dot -T png $< > $@

all: euler-j5-a.png euler-j5-b.png euler-j7.png euler-wtp.png

clean:
	rm -f *.png
