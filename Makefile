all: deco

.PHONY: clean run deco

deco:
	ghc --make Main
	mv Main deco

run:
	./deco

clean:
	rm Main *.hi *.o