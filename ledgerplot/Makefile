GHC=ghc
GHC_OPT=-O2 --make -no-hs-main -optl '-shared' -optc '-DMODULE=LedgerPlot'


ledgerplot: ledgerplot.hs
	$(GHC) -o ledgerplot ledgerplot.hs

clean:
	rm -f *.hi *.o ledgerplot
