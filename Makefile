all: deco

.PHONY: clean run deco

deco: dist/setup-config
	cabal build

dist/setup-config:
	cabal configure

run:
	./dist/build/deco/deco

clean:
	rm -rf dist
