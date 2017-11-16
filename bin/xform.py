#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import absolute_import, division, print_function, unicode_literals

import sys, os, hashlib
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))
import fasin

from fasin import parsimonious
from parsimonious import Grammar, NodeVisitor

cgrammar_spec = r"""
    source          = line*
    line            = cmark_only / cmark_left / cmark_right / cmark_both /
                      comment_only / text_only / text_comment
    cmark_only      = _ "&" _ EOL
    cmark_left      = _ "&" text comment? EOL
    cmark_both      = _ "&" text "&" text EOL
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
    def __init__(self, index, cdm):
        self.index = index
        self.cdm = cdm
        self.buf = []

    def generic_visit(self, node, visited_children):
        #import pdb; pdb.set_trace()
        #print('GGG', node.expr.name, node.text)
        pass

    def visit_cmark_only(self, node, visited_children):
        import pdb; pdb.set_trace()
        #print(node.expr.name, node.text)

    def visit_cmark_left(self, node, visited_children):
        import pdb; pdb.set_trace()
        #print(node.expr.name, node.text)

    def visit_cmark_right(self, node, visited_children):
        import pdb; pdb.set_trace()
        #print(node.expr.name, node.text)

    def visit_cmark_both(self, node, visited_children):
        import pdb; pdb.set_trace()
        #print(node.expr.name, node.text)

    def visit_comment_only(self, node, visited_children):
        import pdb; pdb.set_trace()
        #print(node.expr.name, node.text)

    def visit_text_only(self, node, visited_children):
        import pdb; pdb.set_trace()
        #print(node.expr.name, node.text)

    def visit_text_comment(self, node, visited_children):
        import pdb; pdb.set_trace()
        #print(node.expr.name, node.text)
        n = len(self.cdm['data'])
        self.cdm['data'][n] = ((itemno, start), (itemno, end), n)
        self.cdm['output'].append(n)

def process_cont_comment(input_cdm):

    cdm = {'input': input_cdm, 'data': {}, 'output': []}

    tree = cgrammar.parse(input_cdm['data'][output])
    out = CVisitor(idx, cdm).visit(tree)

# continuation, comment, include, string, format
for path in sys.argv[1:]:
    with open(path, 'r') as f:
        src = f.read()
        cdm = {'input': {'output': path}, 'imap': {(0, len(path)): 'exists((0, %d))'%len(src)}, 'output': src}
        cdm = process_cont_comment(cdm)
