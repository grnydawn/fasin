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

    def showtree(self):
        #print(self.tostr(edgestr='[{name}: {text}]', joinstr=''))
        #print(self.tostr(edgestr='[{name}: {text}]', joinstr='', branchstr='\n[{name}: {text}]'))
        print(self.tostr())

    def tostr(self, edgestr='{text}', joinstr='', branchstr='{text}', generator=None):
        if generator is None:
            generator = lambda child: child.tostr(edgestr=edgestr, joinstr=joinstr, branchstr=branchstr)

        if self.children:
            text = joinstr.join([ generator(c) for c in self.children])
            return branchstr.format(text=text, depth=self.depth, name=self.node.expr_name)
        else:
            return edgestr.format(text=self.node.text, depth=self.depth, name=self.node.expr_name)


def generate_tree(node, parent=None, depth=0):
    children = []
    for n in node:
        children.append(generate_tree(n, parent=node, depth=depth+1))
    return Node(parent, node, children, depth)

class Grammar(pGrammar):

    def parse(self, text, pos=0):
        parse_tree = super(Grammar, self).parse(text, pos=pos)
        tree = generate_tree(parse_tree)
        #import pdb; pdb.set_trace()
        return tree

