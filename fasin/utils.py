# -*- coding: utf-8 -*-
from __future__ import (absolute_import, division,
    print_function, unicode_literals)
import os

here = os.path.dirname(os.path.realpath(__file__))

F77_extensions = ['.f', '.for', '.fpp', '.ftn',
    '.F', '.FOR', '.FPP', '.FTN']

SMAPSTR = '@S%d@'
CMAPSTR = '@C%d@'
FMAPSTR = '@F%d@'

def R2C(rulename):
    splitted = [r if r else '_' for r in rulename.split('_')]
    replaced = [c[0].upper()+c[1:] for c in splitted]
    return ''.join(replaced)

def N2R(obj):
    clsname = obj.__class__.__name__
    rulename = ''.join(['_'+c.lower() if c.isupper() else c for c in clsname])
    return rulename[1:] if rulename[0] == '_' else rulename
