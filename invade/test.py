from ctypes import *

lib = cdll.LoadLibrary('./lib-calculator_finance.so')

print('Test...')

#source:
#http://wiki.python.org/moin/PythonVsHaskell

api = {
#    name               restype         argtypes    input  expected value
    'calcStoploss':     (c_double,      [c_double, c_int, c_double, c_double, c_double, c_double], ([2500.0, 99, 0.25, 7.50, 2.0, 100000.0], 0.0))
    ,'test': (c_double, c_double)
}

for func in api:
    f = getattr(lib, func)
    f.restype, f.argtype, test = api[func]
    input, expected = test
    assert f(input) == expected
    print("test: func,input,expected =",func, ",", input, ",", expected)

#print("test input=", input)
#print("test f.restype=", f.restype)
#print("test f.argtype=", f.argtype)
#assert f(input)
