# This file was automatically created by FeynRules 2.3.49
# Mathematica version: 14.3.0 for Mac OS X ARM (64-bit) (July 8, 2025)
# Date: Tue 24 Nov 2026 04:08:23


from .object_library import all_lorentz, Lorentz

from .function_library import complexconjugate, re, im, csc, sec, acsc, asec, cot
try:
   import form_factors as ForFac 
except ImportError:
   pass


SSSS1 = Lorentz(name = 'SSSS1',
                spins = [ 1, 1, 1, 1 ],
                structure = '1')

SSSS2 = Lorentz(name = 'SSSS2',
                spins = [ 1, 1, 1, 1 ],
                structure = 'P(-1,1)**2 + P(-1,2)**2 + P(-1,3)**2 + P(-1,4)**2')

SSSSSS1 = Lorentz(name = 'SSSSSS1',
                  spins = [ 1, 1, 1, 1, 1, 1 ],
                  structure = '1')

FM2L1 = Lorentz(name='FM2L1', spins=[1, 1], structure='P(-1,1)*P(-1,1)*P(-2,1)*P(-2,1)')

FM2L2 = Lorentz(name='FM2L2', spins=[1, 1], structure='P(-1,1)*P(-1,1)')

FM2L3 = Lorentz(name='FM2L3', spins=[1, 1], structure='1')
