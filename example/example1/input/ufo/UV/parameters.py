# This file was automatically created by FeynRules 2.3.49
# Mathematica version: 14.3.0 for Mac OS X ARM (64-bit) (July 8, 2025)
# Date: Tue 24 Nov 2026 04:08:18



from .object_library import all_parameters, Parameter


from .function_library import complexconjugate, re, im, csc, sec, acsc, asec, cot

# This is a default parameter object representing 0.
ZERO = Parameter(name = 'ZERO',
                 nature = 'internal',
                 type = 'real',
                 value = '0.0',
                 texname = '0')

# User-defined parameters.
fmp1 = Parameter(name = 'fmp1',
                 nature = 'external',
                 type = 'real',
                 value = 1000,
                 texname = '\\text{fmp1}',
                 lhablock = 'FM2INPUT',
                 lhacode = [ 1 ])

fmp2 = Parameter(name = 'fmp2',
                 nature = 'external',
                 type = 'real',
                 value = 1,
                 texname = '\\text{fmp2}',
                 lhablock = 'FM2INPUT',
                 lhacode = [ 2 ])

fmp3 = Parameter(name = 'fmp3',
                 nature = 'external',
                 type = 'real',
                 value = 0.1,
                 texname = '\\text{fmp3}',
                 lhablock = 'FM2INPUT',
                 lhacode = [ 3 ])

fmp4 = Parameter(name = 'fmp4',
                 nature = 'external',
                 type = 'real',
                 value = 0.3,
                 texname = '\\text{fmp4}',
                 lhablock = 'FM2INPUT',
                 lhacode = [ 4 ])

fmp5 = Parameter(name = 'fmp5',
                 nature = 'external',
                 type = 'real',
                 value = 200,
                 texname = '\\text{fmp5}',
                 lhablock = 'FM2INPUT',
                 lhacode = [ 5 ])

fmp1 = Parameter(name = 'fmp1',
                 nature = 'external',
                 type = 'real',
                 value = 1000,
                 texname = '\\text{fmp1}',
                 lhablock = 'MASS',
                 lhacode = [ 9000001 ])

fmp2 = Parameter(name = 'fmp2',
                 nature = 'external',
                 type = 'real',
                 value = 1,
                 texname = '\\text{fmp2}',
                 lhablock = 'MASS',
                 lhacode = [ 9000002 ])

