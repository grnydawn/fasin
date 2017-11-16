# -*- coding: utf-8 -*-
from __future__ import absolute_import, division, print_function, unicode_literals
import os, sys, re
from . import utils

def cpp_perror(msg):
    print('fatal: {}'.format(msg))
    sys.exit()

#def _getmap(linemap, start, end, multiline=False):
#    accumlen = 0
#    alist = []
#    for amap in linemap:
#        if multiline:
#            if accumlen == 0:
#                if start >= amap[0] and start < amap[1]:
#                    if end <= amap[1]:
#                        return amap
#                    else:
#                        accumlen = amap[1] - start
#                        alist.append(amap)
#            else:
#                if amap[1]-amap[0]+accumlen >= end - start:
#                    alist.append(amap)
#                    return alist
#                else:
#                    accumlen += amap[1] - amap[0]
#                    alist.append(amap)
#        else:
#            if start >= amap[0] and end <= amap[1]:
#                return amap
#    prep_error('Can not find a linemap between ({:d}, {:d}) from {}'.format(start, end, linemap))
#
#def stringmap(smap, line, linemap):
#    quote = None
#    index = None
#    newline = []
#    skipnext = False
#    for idx, ch in enumerate(line):
#        if skipnext:
#            skipnext = False
#            continue
#        if ch=='"' or ch=="'":
#            if quote:
#                if quote==ch:
#                    if idx+1 < len(line) and line[idx+1] == quote:
#                        skipnext = True
#                    else:
#                        amap = _getmap(linemap, index, idx, multiline=True)
#                        name = '{}{:d}'.format(utils.SMAPSTR, len(smap))
#                        if isinstance(amap[1], list):
#                            amap[0][1] += len(name) - idx + index
#                            newline.append(name)
#                            smap[name] = line[index:idx]
#                            for remained_map in amap[1:]:
#                                remained_map[0] = amap[0][1] - 1
#                                remained_map[1] = amap[0][1] - 1
#                        else:
#                            amap[1] += len(name) - idx + index
#                            newline.append(name)
#                            smap[name] = line[index:idx+1]
#                        quote = None
#                        index = None
#            else:
#                quote = ch
#                index = idx
#        elif quote is None:
#            newline.append(ch)
#    return ''.join(newline)
#
#def commentmap(cmap, line, linemap):
#    pos = line.find('!')
#    if pos >= 0:
#        amap = _getmap(linemap, pos, len(line))
#        name = '{}{:d}'.format(utils.CMAPSTR, len(cmap))
#        cmap[name] = line[pos:]
#        amap[1] += len(name) - len(line) + pos
#        line = line[:pos] + name
#    return line
#
#
#def splitstmts(line, linemap):
#    stmts = line.split(';')
#    if len(stmts) == 1:
#        return line
#    else:
#        splitted = []
#        index = 0
#        for stmt in stmts:
#            amap = _getmap(linemap, index, index+len(stmt), multiline=True)
#            if isinstance(amap, list):
#                #splitted.append([stmt, [[0, len(stmt), amap[2], index, index+len(stmt)]]])
#                splitted.append([stmt, amap])
#                # TODO: implement this
#                #import pdb; pdb.set_trace()
#            else:
#                splitted.append([stmt, [[0, len(stmt), amap[2], index, index+len(stmt)]]])
#            index += len(stmt) + 1
#        return splitted
#
#def getincpath(pathstr):
#    assert len(pathstr)>2
#    ch1 = pathstr[0]; ch2 = pathstr[-1]
#    assert ch1==ch2 and ch1 in ["'", '"']
#    return pathstr[1:-1].replace(ch1+ch1, ch1)
#
#def process_include(line, new2old, newidx, newlines, jtree, isstrict):
#    incsmap = {}
#    incn2o = [list(new2old[newidx][0])]
#    stringmap(incsmap, line, incn2o)
#    incpath = getincpath(incsmap['{}0'.format(utils.SMAPSTR)])
#    with open(incpath, 'r') as f:
#        inclines = f.read().split('\n')
#    incjson = prepfreeform(inclines, isstrict)
#    del newlines[-1]
#    incnewlines = incjson['newlines']
#    for skey in sorted(incjson['stringmap']):
#        newkey = '{}{:d}'.format(utils.SMAPSTR, len(jtree['stringmap']))
#        for incidx in range(len(incnewlines)):
#            incnewlines[incidx] = incnewlines[incidx].replace(skey, newkey)
#        jtree['stringmap'][newkey] = incjson['stringmap'][skey]
#    for ckey in sorted(incjson['commentmap']):
#        newkey = '{}{:d}'.format(utils.CMAPSTR, len(jtree['commentmap']))
#        for incidx in range(len(incnewlines)):
#            incnewlines[incidx] = incnewlines[incidx].replace(ckey, newkey)
#        jtree['commentmap'][newkey] = incjson['commentmap'][ckey]
#    for incidx, incline in enumerate(incnewlines):
#        newlines.append(incline)
#        new2old[newidx] = [[0, len(incline), incidx, incpath, incjson]]
#        newidx = len(newlines)
#
#def prepfreeform(lines, isstrict):
#
#    jtree = { 'oldlines': lines[:], 'newlines': [], 'old2new': {}, 'new2old': {},
#        'stringmap': {}, 'commentmap': {} }
#
#    oldlines = jtree['oldlines']
#    newlines = jtree['newlines']
#    old2new  = jtree['old2new']
#    new2old  = jtree['new2old']
#
#    buflines = []
#    handle_buflines = False
#    for idx, line in enumerate(oldlines):
#        trimmed = line.strip()
#        if trimmed  == '&':
#            prep_error('"&" can not be the only nonblank character in a line.')
#        posa = line.find('&')
#        if posa >= 0:
#            pose = line.find('!', posa)
#            if pose >= 0 and trimmed[0] == '&' and line[posa+1:pose].strip() == '':
#                prep_error('"&" can not be the only nonblank character before an "!".')
#            pose = line.find('!')
#            if pose < 0 or posa < pose:
#                if buflines:
#                    posx = line.rfind('&')
#                    if posx == posa:
#                        buflines.append((idx, posa+1, len(line), line[posa+1:]))
#                        handle_buflines = True
#                    elif posx > posa:
#                        buflines.append((idx, posa+1, len(line), line[posa+1:posx]))
#                else:
#                    buflines.append((idx, 0, posa, line[:posa]))
#
#        if buflines:
#            if handle_buflines:
#                newidx = len(newlines)
#                new2old[newidx] = []
#                newlines.append('')
#                for oldidx, oldstart, oldend, oldline in buflines:
#                    newstart = len(newlines[-1])
#                    newlines[-1] += oldline
#                    newend = len(newlines[-1]) + 1
#                    new2old[newidx].append([newstart, newend, oldidx, oldstart, oldend])
#                buflines = []
#                incmatch = re.match(r'^\s*include\s+', line, re.I)
#                if incmatch:
#                    prep_error('"include" line can not be continued.')
#                    #process_include(line, new2old, newidx, newlines, jtree, isstrict)
#                else:
#                    newlines[-1] = stringmap(jtree['stringmap'], newlines[-1], new2old[newidx])
#                    newlines[-1] = commentmap(jtree['commentmap'], newlines[-1], new2old[newidx])
#                    splitted = splitstmts(newlines[-1], new2old[newidx])
#                    if isinstance(splitted, list):
#                        del newlines[-1]
#                        for newline, newmap in splitted:
#                            new2old[newidx] = []
#                            newlines.append(newline)
#                            new2old[newidx].extend(newmap)
#                            newidx = len(newlines)
#                    else:
#                        newlines[-1] = splitted
#                handle_buflines = False
#        else:
#            newidx = len(newlines)
#            new2old[newidx] = [[0, len(line), idx, 0, len(line)]]
#            newlines.append(line)
#            incmatch = re.match(r'^\s*include\s+', line, re.I)
#            if incmatch:
#                process_include(line, new2old, newidx, newlines, jtree, isstrict)
#            else:
#                newlines[-1] = stringmap(jtree['stringmap'], newlines[-1], new2old[newidx])
#                newlines[-1] = commentmap(jtree['commentmap'], newlines[-1], new2old[newidx])
#                splitted = splitstmts(newlines[-1], new2old[newidx])
#                if isinstance(splitted, list):
#                    del newlines[-1]
#                    for newline, newmap in splitted:
#                        new2old[newidx] = []
#                        newlines.append(newline)
#                        new2old[newidx].extend(newmap)
#                        newidx = len(newlines)
#                else:
#                    newlines[-1] = splitted
#    return jtree
#
#def preprocess(lines, isfree, isstrict):
#    if isfree is True or (isfree is None and isstrict is not True):
#        return prepfreeform(lines, isstrict)
#    elif isfree is None:
#        print('Please specify Fortran source form.')
#    else:
#        print('Fixed-form is not supported yet.')

def handle_continuation(prevjtree):
    import pdb; pdb.set_trace()
    data = prevjtree['data']

    buf = []
    for idx, ch in enumerate(data):
        if ch == '\\':
            if idx < len(data):
                if data[idx+1] == '\n':
                    pass
            else:
                pass
        else:
            buf.append(ch)
def cpp(path):

    # read
    jtree = { '__map__': None, 'data': open(path, 'r').read()}

    # handle continuation
    jtree = handle_continuation(jtree)
    
    # handle #include

    # generate stringmap

    # generate commentmap

    # parse

    # preprocess
