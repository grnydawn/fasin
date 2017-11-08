# -*- coding: utf-8 -*-
from __future__ import (absolute_import, division,
    print_function, unicode_literals)
import os, sys, time, datetime

here = os.path.dirname(os.path.realpath(__file__))

fixedform_extensions = ['.f', '.for', '.fpp', '.ftn',
    '.F', '.FOR', '.FPP', '.FTN']
freeform_extensions = ['.f90', '.f95', '.f03', '.f08',
    '.F90', '.F95', '.F03', '.F08']

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


def to_bytes(s, encoding='utf-8'):
    try:
        return s.encode(encoding)
    except:
        return s

def to_unicodes(s, encoding=None):
    try:
        if encoding is None:
            encoding = sys.stdin.encoding
        return s.decode(encoding)
    except:
        return s

def datetimestr():
    ts = time.time()
    utc = datetime.datetime.utcfromtimestamp(ts)
    now = datetime.datetime.fromtimestamp(ts)
    tzdiff = now - utc
    secdiff = int(tzdiff.days*24*3600 + tzdiff.seconds)
    tzstr = '{0}{1}'.format('+' if secdiff>=0 else '-',
        time.strftime('%H:%M:%S', time.gmtime(abs(secdiff))))
    return '{0} {1}'.format(str(now), tzstr)

