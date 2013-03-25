CalculatorFinance.so: CalculatorFinance.hs
	ghc -O2 --make -no-hs-main -optl '-shared' -optc '-DMODULE=CalculatorFinance' -o CalculatorFinance.so CalculatorFinance.hs module_init.c
#	$(HC) $(HCFLAGS) --make \
#		-no-hs-main -optl '-shared' -optc '-DMODULE='$(*F) \
#		-o $@ $< module_init.c
