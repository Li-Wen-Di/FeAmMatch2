# This file was automatically created by FeynRules 2.3.49
# Mathematica version: 14.3.0 for Mac OS X ARM (64-bit) (July 8, 2025)
# Date: Tue 24 Nov 2026 04:08:18


from .object_library import all_vertices, Vertex
from . import particles as P
from . import couplings as C
from . import lorentz as L
V_1 = Vertex(name = 'V_1',
             particles = [ P.fmf2, P.fmf2, P.fmf2, P.fmf2 ],
             color = [ '1' ],
             lorentz = [ L.SSSS1 ],
             couplings = {(0,0):C.GC_1})

V_2 = Vertex(name = 'V_2',
             particles = [ P.fmf1, P.fmf1, P.fmf2, P.fmf2 ],
             color = [ '1' ],
             lorentz = [ L.SSSS1 ],
             couplings = {(0,0):C.GC_2})

V_3 = Vertex(name = 'V_3',
             particles = [ P.fmf1, P.fmf2, P.fmf2 ],
             color = [ '1' ],
             lorentz = [ L.SSS1 ],
             couplings = {(0,0):C.GC_3})
