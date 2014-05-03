all: setup build

setup:
	cabal sandbox init
	cabal install --only-dependencies

build:
	cabal build

release: build
	cp dist/build/arashi/arashi .
	strip --strip-unneeded arashi
	upx arashi

clean:
	rm -f arashi

clean-all:
	rm -rf dist .cabal-sandbox arashi
