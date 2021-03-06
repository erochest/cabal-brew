
FLAGS=--enable-tests --allow-newer

all: init test docs package

init:
	cabal sandbox init
	make deps

test: build
	cabal test --test-option=--color

specs: build
	./dist/build/cabal-brew-specs/cabal-brew-specs

run:
	cabal run

# docs:
# generate api documentation
#
# package:
# build a release tarball or executable
#
# dev:
# start dev server or process. `vagrant up`, `yesod devel`, etc.
#
# deploy:
# prep and push

install: test
	cabal install

hlint:
	hlint *.hs CabalBrew specs

clean:
	cabal clean

distclean: clean
	cabal sandbox delete

configure: clean
	cabal configure ${FLAGS}

deps: clean
	cabal install --only-dependencies --allow-newer ${FLAGS}
	make configure

build:
	cabal build

rebuild: clean configure build

.PHONY: all init test run clean distclean configure deps build rebuild hlint
