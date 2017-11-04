# -*- coding: utf-8 -*-
from __future__ import absolute_import, division, print_function, unicode_literals

import sys, os
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '.')))
from parsimonious import Grammar as pGrammar
from parsimonious import NodeVisitor as pNodeVisitor

#class Treeutil(object):
#    @classmethod
#    def _visit(cls, istopdown, node, parent=None, bag=None, depth=0, **actions):
#        if bag is None:
#            bag = {}
#            for action, method in actions.items():
#                if not callable(method):
#                    raise Exception('{} is not callable.'.format(action))
#                bag[action] = [] 
#        try:
#            if istopdown:
#                for action, method in actions.items():
#                    method(parent, node, depth, bag)
#                for n in node:
#                    bag = cls._visit(True, n, parent=node, bag=bag, depth=depth+1, **actions)
#            else:
#                for n in node:
#                    bag = cls._visit(False, n, parent=node, bag=bag, depth=depth+1, **actions)
#                for action, method in actions.items():
#                    method(parent, node, depth, bag)
#        except:
#            raise
#        return bag
#
#    @classmethod
#    def topdown_visit(cls, node, bag=None, **actions):
#        return cls._visit(True, node, bag=bag, **actions)
#
#    @classmethod
#    def bottomup_visit(cls, node, bag=None, **actions):
#        return cls._visit(False, node, bag=bag, **actions)
#
class Node(object):
    smap = None
    cmap = None
    fmap = None
    def __init__(self, parent, node, children, depth):
        self.parent = parent
        self.node = node
        self.children = children
        self.depth = depth

    @staticmethod
    def is_blanknode(obj):
        if obj.node.expr.name in ['_0', '_1', '_L', '_CL', '_B', '_S', 'EOL']:
            return True
        else:
            return False

    def __str__(self):
        return self.tostr()

    def showtree(self):
        def indent(text):
            return '\n'.join(('  ' + line) for line in text.splitlines())
        def nodestr(node):
            text = u'"%s"' % node.node.text if len(node.node.children)==0 else ''
            return u'%s:%s' % (node.node.expr_name, text)
        print(self.tostr(text=nodestr, skip=self.is_blanknode, control=indent, joinstr='\n'))

    def applymaps(self, text):
        for mapping in [self.smap, self.cmap, self.fmap]:
            if isinstance(mapping, dict):
                for k, v in mapping.items():
                    text = text.replace(k, v)
        return text

    def tostr(self, text=None, skip=None, control=None, joinstr='', depth=0):
        if skip and skip(self):
            return ''
        if text:
            ret = [text(self)]
        else:
            ret = ['%s' % self.node.text if len(self.node.children)==0 else '']
        for n in self.children:
            if control:
                ret.append(control(n.tostr(text=text, skip=skip, control=control, joinstr=joinstr, depth=depth+1)))
            else:
                ret.append(n.tostr(text=text, skip=skip, control=control, joinstr=joinstr, depth=depth+1))
        outstr = joinstr.join([r for r in ret if r])
        return self.applymaps(outstr) if depth==0 else outstr

def generate_tree(node, parent=None, depth=0, lift_child=True, remove_blanknode=True):
    if node.expr.__class__.__name__ == 'Optional':
        return generate_tree(node.children[0], parent=node, depth=depth)
    children = [_n for _n in node if (not remove_blanknode) or _n.start!=_n.end]
    if lift_child and len(children) == 1:
        return generate_tree(children[0], parent=node, depth=depth+1)
    else:
        return Node(parent, node, [generate_tree(child, parent=node, depth=depth+1)
            for child in children], depth)

class Grammar(pGrammar):

    def parse(self, preprocessed, pos=0):
        parse_tree = super(Grammar, self).parse('\n'.join(preprocessed['newlines']), pos=pos)
        tree = generate_tree(parse_tree, lift_child=True)
        tree.smap = preprocessed['stringmap']
        tree.cmap = preprocessed['commentmap']
        tree.fmap = preprocessed['formatmap']
        #import pdb; pdb.set_trace()
        return tree

