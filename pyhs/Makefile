GHC=ghc
GHC_RUNTIME_LINKER_FLAG=-lHSrts-ghc7.4.2.so

libffi-CalculatorFinance.so: CalculatorFinance.o wrapper.o
$(GHC) -o $@ -shared -dynamic -fPIC $^ $(GHC_RUNTIME_LINKER_FLAG)

CalculatorFinance_stub.h CalculatorFinance.o: CalculatorFinance.hs
$(GHC) -c -dynamic -fPIC CalculatorFinance.hs

wrapper.o: wrapper.c CalculatorFinance_stub.h
$(GHC) -c -dynamic -fPIC wrapper.c

clean:
rm -f *.hi *.o *_stub.[ch]

clean-all:
rm -f *.hi *.o *_stub.[ch] *.so

# Runs the example Python program
test: libffi-CalculatorFinance.so
python test.py
