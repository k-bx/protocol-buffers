build:
	cabal install --enable-tests
	cabal configure --enable-tests
	cabal test
	cd descriptor && cabal install --enable-tests
	cd descriptor && cabal configure --enable-tests
	cd descriptor && cabal test
	cd hprotoc && cabal install --enable-tests
	cd hprotoc && cabal configure --enable-tests
	cd hprotoc && cabal test
.PHONY: build
