# -*- coding: utf-8 -*-
from __future__ import absolute_import, division, print_function, unicode_literals
from functools import partial
import interval


###############################################
#   {
#       'input': input chain
#       'imap': inverse mapping EX: {0: 'E(0) or E(2)', 1: 'E(1) or E(2)', 2: 'E(0,1)'}
#       'data': data EX: { 0: 'Red', 1: 'Yellow', 2:'Blue' }
#       'output': indices used EX: [0, 1, 2]
#   }

class span(interval.interval):
    '''This is a placeholder for future modification
    for supporting integer-only indexing
    '''
    def indices(self):
        for idx in self:
            inf = int(idx.inf)
            sup = int(idx.sup)
            for i in range(inf, sup+1):
                yield i
#        try:
#            item = self.lst[self.idx]
#        except IndexError:
#            raise StopIteration()
#        self.idx += 1
#        return item


def _exists(ranges, r):
    return r in ranges

def collect_data(data, ranges):
    collected = []
    for s, e in ranges:
        for idx in range(int(s),int(e+1)):
            try:
                collected.append(data['data'][idx])
            except: import pdb; pdb.set_trace()
    return collected

def intspan(r, ranges):
    start = stop = r
    if r-1 in ranges:
        start = r - 1
    if r+1 in ranges:
        start = r + 1
    return span((start, stop))

def normalize(r, ranges):
    if isinstance(r, int):
        return intspan(r, ranges)
    elif isinstance(r, span):
        return r
    else:
        raise Exception('%s is not supported.'%r.__class__.__name__)

def restore(mapping, ranges=None, depth=1):
    depth -= 1

    if ranges is None:
        ranges = mapping['output']

    if isinstance(ranges, int):
        ranges = span(ranges)
    elif not isinstance(ranges, span):
        _ranges = span()
        try:
            for r in ranges:
                _ranges |= normalize(r, _ranges)
        except:
            raise Exception('%s is not supported.'%ranges.__class__.__name__)
        ranges = _ranges

    #import pdb; pdb.set_trace()
    if 'imap' in mapping and mapping['imap'] and depth > 0:
        exists = partial(_exists, ranges)
        local_dict = {'exists':exists, 'E':exists}
        restored_ranges = span()
        for r, expr in mapping['imap'].items():
            if eval(expr, globals(), local_dict):
                restored_ranges |= normalize(r, restored_ranges)
        return restore(mapping['input'], restored_ranges, depth=depth)
    else:
        return collect_data(mapping, ranges)
