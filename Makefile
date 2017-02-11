hardwords: src/Hardwords.hs
	mkdir -p build; ghc -O3 -o hardwords -hidir build -odir build --make src/Hardwords.hs
