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
    def __init__(self, parent, node, children, depth):
        self.parent = parent
        self.node = node
        self.children = children
        self.depth = depth

    @staticmethod
    def is_blanknode(obj):
        if obj.node.expr.name in ['_0', '_1', '_L', '_CL', '_B', 'EOL']:
            return True
        else:
            return False

    def showtree(self):
        def indent(text):
            return '\n'.join(('  ' + line) for line in text.splitlines())
        print(self.tostr(skip=self.is_blanknode, control=indent))

    def tostr(self, text=None, skip=None, control=None, joinstr='\n'):
        if skip and skip(self):
            return ''
        if text:
            pass
        else:
            text = '"%s"' % self.node.text if len(self.node.children)==0 else ''
            ret = [u'%s:%s' % (self.node.expr_name, text)]
        for n in self.children:
            if control:
                ret.append(control(n.tostr(text=text, skip=skip, control=control)))
            else:
                ret.append(n.tostr(text=text, skip=skip, control=control))
        return joinstr.join([ r for r in ret if r])


def generate_tree(node, parent=None, depth=0, lift_child=True, remove_blanknode=True):
    children = [_n for _n in node if (not remove_blanknode) or _n.start!=_n.end]
    if lift_child and len(children) == 1:
        return generate_tree(children[0], parent=node, depth=depth+1)
    else:
        return Node(parent, node, [generate_tree(child, parent=node, depth=depth+1)
            for child in children], depth)

class Grammar(pGrammar):

    def parse(self, text, pos=0):
        parse_tree = super(Grammar, self).parse(text, pos=pos)
        tree = generate_tree(parse_tree, lift_child=True)
        #import pdb; pdb.set_trace()
        return tree

