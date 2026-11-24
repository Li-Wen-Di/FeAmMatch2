# This file was automatically created by FeynRules 2.3.49
# Mathematica version: 14.3.0 for Mac OS X ARM (64-bit) (July 8, 2025)
# Date: Tue 24 Nov 2026 04:08:18


from .object_library import all_decays, Decay
from . import particles as P
Decay_fmf1 = Decay(name = 'Decay_fmf1',
                   particle = P.fmf1,
                   partial_widths = {(P.fmf2,P.fmf2):'(fmp5**2*cmath.sqrt(fmp1**4 - 4*fmp1**2*fmp2**2))/(32.*cmath.pi*abs(fmp1)**3)'})

