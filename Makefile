HC=cabal
VERFLAGS=ghc-9.12.2
all:
	$(HC) run --with-compiler=$(VERFLAGS)
build:  
	$(HC) build --with-compiler=$(VERFLAGS)
