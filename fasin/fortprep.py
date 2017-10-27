# -*- coding: utf-8 -*-
from __future__ import absolute_import, division, print_function, unicode_literals
import os, sys
from . import utils

def preperror(msg):
    print('fatal: {}'.format(msg))
    sys.exit()

def _getmap(linemap, start, end, multiline=False):
    accumlen = 0
    alist = []
    for amap in linemap:
        if multiline:
            if accumlen == 0:
                if start >= amap[0] and start < amap[1]:
                    if end <= amap[1]:
                        return amap
                    else:
                        accumlen = amap[1] - start
                        alist.append(amap)
            else:
                if amap[1]-amap[0]+accumlen >= end - start:
                    alist.append(amap)
                    return alist
                else:
                    accumlen += amap[1] - amap[0]
                    alist.append(amap)
        else:
            if start >= amap[0] and end <= amap[1]:
                return amap
    preperror('Can not find a linemap between ({:d}, {:d}) from {}'.format(start, end, linemap))

def stringmap(smap, line, linemap):
    quote = None
    index = None
    newline = []
    for idx, ch in enumerate(line):
        if ch=='"' or ch=="'":
            if quote:
                if quote==ch:
                    amap = _getmap(linemap, index, idx, multiline=True) 
                    name = '{}{:d}'.format(utils.SMAPSTR, len(smap))
                    if isinstance(amap[1], list):
                        amap[0][1] += len(name) - idx + index
                        newline.append(name)
                        smap[name] = line[index:idx]
                        for remained_map in amap[1:]:
                            remained_map[0] = amap[0][1] - 1 
                            remained_map[1] = amap[0][1] - 1 
                    else:
                        amap[1] += len(name) - idx + index
                        newline.append(name)
                        smap[name] = line[index:idx]
                    quote = None
                    index = None
            else:
                quote = ch
                index = idx
        elif quote is None:
            newline.append(ch)
    return ''.join(newline)

def commentmap(cmap, line, linemap):
    pos = line.find('!')
    if pos >= 0:
        amap = _getmap(linemap, pos, len(line)) 
        name = '{}{:d}'.format(utils.CMAPSTR, len(cmap))
        cmap[name] = line[pos:]
        amap[1] += len(name) - len(line) + pos
        line = line[:pos] + name
    return line


def splitstmts(line, linemap):
    stmts = line.split(';')
    if len(stmts) == 1:
        return line
    else:
        splitted = []
        index = 0
        for stmt in stmts:
            amap = _getmap(linemap, index, index+len(stmt)) 
            splitted.append([stmt, [0, len(stmt), amap[2], index, index+len(stmt)]])
            index += len(stmt) + 1
        return splitted

def prepfreeform(lines, isstrict):

    jtree = { 'oldlines': lines[:], 'newlines': [], 'old2new': {}, 'new2old': {},
        'stringmap': {}, 'commentmap': {} }

    oldlines = jtree['oldlines']
    newlines = jtree['newlines']
    old2new  = jtree['old2new']
    new2old  = jtree['new2old']

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
            if buflines:
                posx = line.rfind('&')
                if posx == posa:
                    buflines.append((idx, posa+1, len(line), line[posa+1:]))
                elif posx > posa:
                    buflines.append((idx, posa+1, len(line), line[posa+1:posx]))
            else:
                buflines.append((idx, 0, posa, line[:posa]))
        elif buflines:
            newidx = len(newlines)
            new2old[newidx] = []            
            newlines.append('')
            for oldidx, oldstart, oldend, oldline in buflines:
                newstart = len(newlines[-1])
                newlines[-1] += oldline
                newend = len(newlines[-1]) + 1
                new2old[newidx].append([newstart, newend, oldidx, oldstart, oldend])
            buflines = []
            newlines[-1] = stringmap(jtree['stringmap'], newlines[-1], new2old[newidx])
            newlines[-1] = commentmap(jtree['commentmap'], newlines[-1], new2old[newidx])
            splitted = splitstmts(newlines[-1], new2old[newidx])
            if isinstance(splitted, list):
                del newlines[-1]
                new2old[newidx] = []
                for newline, newmap in splitted:
                    newidx = len(newlines)
                    newlines.append(newline)
                    new2old[newidx].append(newmap)
            else:
                newlines[-1] = splitted
        else:
            newidx = len(newlines)
            new2old[newidx] = [[0, len(line), idx, 0, len(line)]]
            newlines.append(line)
            newlines[-1] = stringmap(jtree['stringmap'], newlines[-1], new2old[newidx])
            newlines[-1] = commentmap(jtree['commentmap'], newlines[-1], new2old[newidx])
            splitted = splitstmts(newlines[-1], new2old[newidx])
            if isinstance(splitted, list):
                del newlines[-1]
                for newline, newmap in splitted:
                    new2old[newidx] = []
                    newlines.append(newline)
                    new2old[newidx].append(newmap)
                    newidx = len(newlines)
            else:
                newlines[-1] = splitted
    return jtree

def preprocess(lines, isfree, isstrict):

    if isfree is True or (isfree is None and isstrict is not True):
        return prepfreeform(lines, isstrict)
    elif isfree is None:
        print('Please specify Fortran source form.')
    else:
        print('Fixed-form is not supported yet.')

def prep(path, isfree=None, isstrict=None):

    _, ext = os.path.splitext(path)
    if isfree is None and isstrict is not True:
        isfree = not ext in utils.F77_extensions

    with open(path, 'r') as f:
        preprocessed = preprocess(f.read().split('\n'), isfree, isstrict)
        #import pdb; pdb.set_trace()
        print('\n'.join(preprocessed['newlines']))
        return preprocessed
