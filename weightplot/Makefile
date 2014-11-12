GHC=ghc
GHC_OPT=-O2 --make -no-hs-main -optl '-shared' -optc '-DMODULE=Main'


weightplot: weightplot.hs
	$(GHC) -o weightplot weightplot.hs

clean:
	rm -f *.hi *.o weightplot
