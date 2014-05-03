all: setup build

setup:
	cabal sandbox init
	cabal install --only-dependencies

build:
	cabal build
	cp dist/build/arashi/arashi .

release: build
	strip --strip-unneeded arashi
	upx arashi

compress: build
	gzip -c arashi > arashi.gz

clean:
	rm -f arashi

clean-all:
	rm -rf dist .cabal-sandbox arashi
