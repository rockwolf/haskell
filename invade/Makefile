#CalculatorFinance.so: CalculatorFinance.hs
#	ghc -O2 --make -no-hs-main -optl '-shared' -optc '-DMODULE=CalculatorFinance' -o CalculatorFinance.so CalculatorFinance.hs module_init.c
#	$(HC) $(HCFLAGS) --make \
#		-no-hs-main -optl '-shared' -optc '-DMODULE='$(*F) \
#		-o $@ $< module_init.c
#

GHC=ghc
#GHC_RUNTIME_LINKER_FLAG=-lHSrts-ghc7.4.2.so
GHC_OPT=-O2 --make -no-hs-main -optl '-shared' -optc '-DMODULE=CalculatorFinance'

lib-calculator_finance.so: CalculatorFinance.o module_init.o
	$(GHC) -o $@ $(GHC_OPT) -dynamic -shared -lHSrts-ghc7.6.2 $^
	#$(GHC) -o $@ $(GHC_OPT) -lHSrts-ghc7.6.2 $^

CalculatorFinance_stub.h CalculatorFinance.o: CalculatorFinance.hs
	$(GHC) -c -dynamic -fPIC CalculatorFinance.hs

module_init.o: module_init.c CalculatorFinance_stub.h
	$(GHC) -c -dynamic -fPIC module_init.c

clean:
	rm -f *.hi *.o *_stub.[ch] *.so

#clean-all:
#	rm -f *.hi *.o *_stub.[ch] *.so

# Runs the example Python program
example: lib-calculator_finance.so
	python test.py
