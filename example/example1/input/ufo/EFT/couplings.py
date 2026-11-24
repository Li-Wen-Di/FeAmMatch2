from .object_library import all_couplings, Coupling
from .function_library import complexconjugate, re, im, csc, sec, acsc, asec, cot
GC_1 = Coupling(name='GC_1', value='-(fmp4*complex(0,1))', order={'QED': 1})
GC_2 = Coupling(name='GC_2', value='-(fmp5*complex(0,1))', order={'QED': 1})
GC_3 = Coupling(name='GC_3', value='(fmp6*complex(0,1))/4.', order={'QED': 1})
FM2GC1 = Coupling(name='FM2GC1', value='((0+complex(0,1)*-1)*fmp7)', order={'QED': 1})
FM2GC2 = Coupling(name='FM2GC2', value='((0+complex(0,1)*1)*(-1+fmp2))', order={'QED': 1})
FM2GC3 = Coupling(name='FM2GC3', value='((0+complex(0,1)*1)*((fmp1)**(2)+(-1*fmp3)))', order={'QED': 1})
