# -*- coding: utf-8 -*-
from __future__ import absolute_import, division, print_function, unicode_literals

here = os.path.dirname(os.path.realpath(__file__))

F77_extensions = ['.f', '.for', '.fpp', '.ftn',
    '.F', '.FOR', '.FPP', '.FTN']

def preperror(msg):
    print(msg)
    sys.exit()

def prepfreeform(lines, isstrict):

    # linespan: ( line#, start#, end# )
    jtree = { 'oldlines': lines[:], 'newlines': [], 'old2new': {}, 'new2old': {},
        'stringmap': {}, 'commentmap': {} }

    oldlines = jtree['oldlines']
    newlines = jtree['newlines']
    buflines = []
    for idx, line in enumerate(oldlines):
        trimmed = line.strip()
        if trimmed  == '&':
            prep_error('"&" can not be the only nonblank character in a line.')
        posa = line.find('&')
        if posa >= 0:
            pose = line.find('!', posa)
            if pose >= 0 and trimmed[0] == '&' and line[posa+1:pose].strip() == '':
                prep_error('"&" can not be the only nonblank character before an "!".')
            buflines.append((idx, 0, posa, line[:posa]))
        elif len(buflines) > 0:
            for i, s, e, l in buflines:
                
            # merge first
            # and split if there are multi-stmts
        else:
            # and split if there are multi-stmts
    
    # prep multi-lines

    # prep multi-stmts


    # prep comments 

    # prep rep-chars

    # prep labels


    return jtree

def prepocess(lines, isfree, isstrict):

    if isfree is True or (isfree is None and isstrict is not True):
        return prepfreeform(lines, isstrict)
    elif isfree is None:
        print('Please specify Fortran source form.')
    else:
        print('Fixed-form is not supported yet.')

def prep(path, isfree=None, isstrict=None):

    _, ext = os.path.splitext(path)
    if isfree is None and isstrict is not True:
        isfree = not ext in F77_extensions:

    with open(path, 'r') as f:
        return preprocess(f.read().split('\n'), isfree, isstrict)
