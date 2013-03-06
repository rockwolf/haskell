from ctypes import *


lib = cdll.LoadLibrary("./libffi-CalculatorFinance.so")

lib.haskell_init()

for x in xrange(10):
    print("Haskell fib(%d): %d" % (x, lib.fibonacci_hs(x)))

lib.haskell_exit()
