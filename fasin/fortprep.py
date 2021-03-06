# -*- coding: utf-8 -*-
from __future__ import absolute_import, division, print_function, unicode_literals
import os, sys, re, six
from . import utils

def prep_error(msg):
    print('fatal: {}'.format(msg))
    sys.exit()

def stringmap(smap, line):
    quote = None # open quote character
    index = None # index of quote
    newline = [] # saving characters and mapnames
    skipnext = False # if double quotes
    for idx, ch in enumerate(line): # per every characters
        if skipnext:
            skipnext = False
            continue
        if ch=='"' or ch=="'":
            if quote: # if opened
                if quote==ch: # if close quote character
                    if idx+1 < len(line) and line[idx+1] == quote: # if double quotes
                        skipnext = True
                    else:
                        if ch=='"':
                            name = utils.SMAPSTR % len(smap)
                        else:
                            name = utils.SMAPSTR % len(smap)
                        newline.append(name)
                        smap[name] = (line[index+1:idx], ch)
                        quote = None
                        index = None
            else:
                quote = ch
                index = idx
        elif quote is None:
            newline.append(ch)
    return ''.join(newline)

def commentmap(cmap, line):
    pos = line.find('!')
    if pos >= 0:
        name = utils.CMAPSTR % len(cmap)
        cmap[name] = line[pos:]
        line = line[:pos] + name
    return line

# TODO: implement this
#Before a slash edit descriptor when the optional repeat specification is not present (10.7.2),
#After a slash edit descriptor, or
#Before or after a colon edit descriptor (10.7.3)
# format, read, write, print
def formatmap(fmap, line):
    pattern = r'^[ \t]*([0-9]{1,5}[ \t])?(?P<stmt>format|read|write|print)(?P<remained>.+)$'
    match = re.match(pattern, line, re.I)
    if match:
        stmt = match.group('stmt').strip().upper()
        remained = match.group('remained').strip().upper()
        if stmt == 'FORMAT':
            pass
        else:
            if remained.startswith('('):
                pos_fmt = remained.find('FMT')
                if pos_fmt > 0:
                    pass
                else:
                    pass
            else:
                pass
        #import pdb; pdb.set_trace()
    return line

def getincpath(pathstr):
    assert len(pathstr)>0
    ch1 = pathstr[0]; ch2 = pathstr[-1]
    if not(ch1==ch2 and ch1 in ["'", '"']):
        import pdb; pdb.set_trace()
    assert ch1==ch2 and ch1 in ["'", '"']
    return pathstr[1:-1].replace(ch1+ch1, ch1)

def process_include(line, newidx, newlines, jtree, isstrict):
    incsmap = {}
    stringmap(incsmap, line)
    incpath, qch = incsmap[utils.SMAPSTR % 0]
    with open(incpath, 'r') as f:
        inclines = f.read().split('\n')
    incjson = prepfreeform(inclines, isstrict)
    del newlines[-1]
    incnewlines = incjson['newlines']
    for skey in sorted(incjson['stringmap']):
        newkey = utils.SMAPSTR % len(jtree['stringmap'])
        for incidx in range(len(incnewlines)):
            incnewlines[incidx] = incnewlines[incidx].replace(skey, newkey)
        jtree['stringmap'][newkey] = incjson['stringmap'][skey]
    for ckey in sorted(incjson['commentmap']):
        newkey = utils.CMAPSTR % len(jtree['commentmap'])
        for incidx in range(len(incnewlines)):
            incnewlines[incidx] = incnewlines[incidx].replace(ckey, newkey)
        jtree['commentmap'][newkey] = incjson['commentmap'][ckey]
    for incidx, incline in enumerate(incnewlines):
        newlines.append(incline)
        newidx = len(newlines)

def prepfreeform(lines, isstrict):

    jtree = {'oldlines':lines[:], 'newlines':[], 'stringmap':{}, 'commentmap':{},
        'formatmap':{}}

    oldlines = jtree['oldlines']
    newlines = jtree['newlines']

    buflines = []
    handle_buflines = False
    for idx, line in enumerate(oldlines):

        # handling continuation marks
        trimmed = line.strip()
        if trimmed  == '&':
            prep_error('"&" can not be the only nonblank character in a line.')
        posa = line.find('&')
        if posa >= 0: # if c-mark is found
            pose = line.find('!', posa)
            if pose >= 0 and trimmed[0] == '&' and line[posa+1:pose].strip() == '':
                prep_error('"&" can not be the only nonblank character before an "!".')
            pose = line.find('!')
            if pose < 0 or posa < pose: # if no comment or comment starts after c-mark
                if buflines: # if continuing c-mark(s)
                    posx = line.rfind('&')
                    if posx == posa:
                        if line[:posa].strip():
                            buflines.append(line[:posa])
                        elif line[posa+1:].strip():
                            buflines.append(line[posa+1:])
                            handle_buflines = True
                        else:
                            prep_error('fatal: unexpected location of continuation mark: {}.'.format(line))
                    elif posx > posa:
                        buflines.append(line[posa+1:posx])
                else: # if start of c-mark
                    buflines.append(line[:posa])
        elif buflines:
            pose = line.find('!')
            if pose < 0 or line[:pose].strip():
                buflines.append(line)
                handle_buflines = True

        if buflines: # if continued
            if handle_buflines:
                newidx = len(newlines)
                newlines.append(''.join(buflines))
                buflines = []
                incmatch = re.match(r'^\s*include\s+', line, re.I)
                if incmatch:
                    prep_error('"include" line can not be continued.')
                else:
                    newlines[-1] = formatmap(jtree['formatmap'], newlines[-1])
                    newlines[-1] = stringmap(jtree['stringmap'], newlines[-1])
                    newlines[-1] = commentmap(jtree['commentmap'], newlines[-1])
                    stmts = newlines[-1].split(';')
                    if len(stmts) > 0:
                        del newlines[-1]
                        newlines.extend(stmts)
                handle_buflines = False

        else: # if not continued
            newidx = len(newlines)
            newlines.append(line)
            incmatch = re.match(r'^\s*include\s+', line, re.I)
            if incmatch:
                process_include(line, newidx, newlines, jtree, isstrict)
            else:
                newlines[-1] = formatmap(jtree['formatmap'], newlines[-1])
                newlines[-1] = stringmap(jtree['stringmap'], newlines[-1])
                newlines[-1] = commentmap(jtree['commentmap'], newlines[-1])
                stmts = newlines[-1].split(';')
                if len(stmts) > 0:
                    del newlines[-1]
                    newlines.extend(stmts)
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
        isfree = not ext in utils.fixedform_extensions
    with open(path, 'r') as f:
        preprocessed = preprocess(f.read().split('\n'), isfree, isstrict)
        #import pdb; pdb.set_trace()
        #print('\n'.join(preprocessed['newlines']))
        return preprocessed

