#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import absolute_import, division, print_function, unicode_literals

import sys, os, hashlib, re
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))
import fasin

from .utils import CMAPSTR, SMAPSTR
from .cfim import span
from parsimonious import Grammar, NodeVisitor

match_include = re.compile(r'\s*include\s+(?P<quote>[\'"])(?P<path>.+)', re.I)

cgrammar_spec = r"""
    source          = line*
    line            = cmark_only / cmark_left / cmark_right / cmark_both /
                      comment_only / text_only / text_comment
    cmark_only      = _ "&" _ EOL
    cmark_left      = _ "&" text comment? EOL
    cmark_both      = _ "&" text "&" comment? EOL
    comment_only    = _ comment EOL
    text_only       = text EOL
    text_comment    = text comment EOL
    cmark_right     = text "&" comment? EOL
    comment         = "!" ~"[^\n]*"
    text            = ~"[^&!\n]*"
    EOL             = "\n"
    _               = ~"[ \t]*"
"""

cgrammar = Grammar(cgrammar_spec)

class CVisitor(NodeVisitor):
    def __init__(self, data, imap):
        self.data = data
        self.imap = imap
        self.output = span()
        self.comments = {}
        self.isopen = None
        self.skipnext = False

    def _check_string_open(self, text):
        _len = len(text)
        for idx, ch in enumerate(text):
            if self.skipnext:
                self.skipnext = False
                continue
            if ch=='\\':
                if self.isopen:
                    self.skipnext = True
            elif ch in ['"', "'"]:
                if idx+1<_len and text[idx+1]==ch and ch==self.isopen:
                    self.skipnext = True
                elif ch==self.isopen:
                    self.isopen = None
                else:
                    self.isopen = ch
        return self.isopen

    def generic_visit(self, node, visited_children):
        pass

    def visit_cmark_only(self, node, visited_children):
        raise Exception('Syntax error: %s'%node.text)

    def visit_cmark_left(self, node, visited_children):
        # children: _ "&" text comment? EOL
        self._append_text(node.children[0], output=False)
        self._append_text(node.children[1], output=False)
        self._append_text(node.children[2], output=True)
        if self._check_string_open(node.children[2].text):
            if node.children[3].children:
                self._append_text(node.children[3], output=True)
        else:
            if node.children[3].children:
                self._append_comment(node.children[3], output=True)
        self._append_text(node.children[4], output=True)

    def visit_cmark_right(self, node, visited_children):
        # chidlren: text "&" comment? EOL
        self._append_text(node.children[0], output=True)
        self._append_text(node.children[1], output=False)
        if self._check_string_open(node.children[0].text):
            if node.children[2].children:
                raise Exception('Comment can not be followed after string open.')
        if node.children[2].children:
            self._append_comment(node.children[2], output=False)
        self._append_text(node.children[3], output=False)

    def visit_cmark_both(self, node, visited_children):
        # children: _ "&" text "&" comment? EOL
        self._append_text(node.children[0], output=False)
        self._append_text(node.children[1], output=False)
        self._append_text(node.children[2], output=True)
        if self._check_string_open(node.children[2].text):
            if node.children[4].children:
                raise Exception('Comment can not be followed after string open.')
        self._append_text(node.children[3], output=False)
        if node.children[4].children:
            self._append_comment(node.children[4], output=False)
        self._append_text(node.children[5], output=False)

    def visit_comment_only(self, node, visited_children):
        # children: _ comment EOL
        self._append_text(node.children[0])
        self._append_comment(node.children[1])
        self._append_text(node.children[2])

    def visit_text_only(self, node, visited_children):
        # children: text EOL
        self._append_text(node.children[0])
        self._append_text(node.children[1])

    def visit_text_comment(self, node, visited_children):
        # children: text comment EOL
        self._append_text(node.children[0])
        self._append_comment(node.children[1])
        self._append_text(node.children[2])

    def _append_text(self, node, output=True):
        idx = len(self.data)
        self.data[idx] = node.text
        if output: self.output |= span(idx)

    def _append_comment(self, node, output=True):
        idx = len(self.data)
        cidx = len(self.comments)
        self.comments[cidx] = node.text
        self.data[idx] = CMAPSTR%cidx
        if output: self.output |= span(idx)

#    def visit_text(self, node, visited_children):
#        pass
#
#    def visit_EOL(self, node, visited_children):
#        pass
#
#    def visit_comment(self, node, visited_children):
#        pass
#
#    def visit_line(self, node, visited_children):
#        pass
#
#    def visit_source(self, node, visited_children):
#        pass

def datajoin(cdm, delim=''):
    srclist = []
    for idx in cdm['output'].indices():
        srclist.append(cdm['data'][idx])
    return delim.join(srclist)

def process_continuation_comment(cdm):
    tree = cgrammar.parse(datajoin(cdm['input']))
    visitor = CVisitor(cdm['data'], cdm['imap'])
    visitor.visit(tree)
    cdm['output'] = visitor.output
    cdm['comments'] = visitor.comments

def process_string_literal(cdm):
    strings = {}
    cdm['strings'] = strings
    cdm['output'] = span()

    text = []
    pindices = []
    for pidx in cdm['input']['output'].indices():
        pindices.append(pidx)
        text.append(cdm['input']['data'][pidx])
        if text[-1] != '\n':
            continue
        text = ''.join(text)
        _len = len(text)
        skipnext = False
        isopen = None
        openidx = None
        newtext = []
        for idx, ch in enumerate(text):
            if skipnext:
                skipnext = False
                continue
            if ch=='\\':
                if isopen:
                    skipnext = True
                else:
                    newtext.append(ch)
            elif ch in ['"', "'"]:
                if idx+1<_len and text[idx+1]==ch and ch==isopen:
                    skipnext = True
                elif ch==isopen:
                    sidx = len(strings)
                    strings[sidx] = (ch, text[openidx:idx])
                    newtext.append(SMAPSTR%sidx)
                    isopen = None
                    openidx = None
                else:
                    isopen = ch
                    openidx = idx + 1
            elif not isopen:
                newtext.append(ch)
        text = []
        didx = len(cdm['data'])
        cdm['data'][didx] = ''.join(newtext)
        cdm['output'] |= span(didx)

    return cdm

def _read_include(line):
    lines = []
    match = match_include.match(line)
    if match is not None:
        quote = match.group('quote')
        path = match.group('path')
        pos = path.find(quote)
        with open(path[:pos], 'r') as f:
            for _line in f.read().split('\n'):
                lines.extend(_read_include(_line))
    else:
        lines.append(line)
    return lines

def process_include(cdm):

    src = datajoin(cdm['input'])

    lines = []
    for line in src.split('\n'):
        lines.extend(_read_include(line))
    cdm['data'][0] = '\n'.join(lines)
    cdm['output'] = span(0)

def process_muliple_statements(cdm):

    output = span()
    for idx in cdm['input']['output'].indices():
        stmts = cdm['input']['data'][idx].split(';')
        for stmt in stmts[:-1]:
            didx = len(cdm['data'])
            cdm['data'][didx] = stmt
            cdm['data'][didx+1] = ';'
            cdm['data'][didx+2] = '\n'
            output |= span(didx)
            output |= span(didx+2)
        didx = len(cdm['data'])
        cdm['data'][didx] = stmts[-1]
        output |= span(didx)
    cdm['output'] = output

def prep(path):
    with open(path, 'r') as f:

        input_data = {'data': {0: f.read()}, 'output': span(0)}

        cdm_i = {'input': input_data, 'data': {}, 'imap': {}, 'output': None}
        process_include(cdm_i)

        cdm_c = {'input': cdm_i, 'data': {}, 'imap': {}, 'output': None}
        process_continuation_comment(cdm_c)

        cdm_s = {'input': cdm_c, 'data': {}, 'imap': {}, 'output': None }
        process_string_literal(cdm_s)

        cdm_m = {'input': cdm_s, 'data': {}, 'imap': {}, 'output': None }
        process_muliple_statements(cdm_m)

        #import pdb; pdb.set_trace()
        return cdm_m

## continuation, comment, include, string, format
#for path in sys.argv[1:]:
#    with open(path, 'r') as f:
#        src = f.read()
#
#        # include
#        cdm_i = {'input': {'data': {0: src}, 'output': span(0)}, 'data': {}, 'imap': {}, 'output': None}
#        process_include(cdm_i)
#
#        cdm_c = {'input': cdm_i, 'data': {}, 'imap': {}, 'output': None}
#        process_cont_comment(cdm_c)
#
#        cdm_s = {'input': cdm_c, 'data': {}, 'imap': {}, 'output': None }
#        process_string_literal(cdm_s)
#        #import pdb; pdb.set_trace()
