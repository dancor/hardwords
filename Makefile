hardwords: src/Hardwords.hs
	mkdir -p build; ghc -O3 -o hardwords -hidir build -odir build \
		-package cryptonite -package optparse-applicative -package entropy \
		--make src/Hardwords.hs
