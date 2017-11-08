# -*- coding: utf-8 -*-
from __future__ import absolute_import, division, print_function, unicode_literals

import sys, os
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '.')))
from parsimonious import Grammar as pGrammar
from parsimonious import NodeVisitor as pNodeVisitor
from . import utils

_cache = {}

class Node(object):
    smap = None
    cmap = None
    fmap = None

    _class_cache = {}

    def __init__(self, parent, node, children, depth):
        self.parent = parent
        self.node = node
        self.children = children if children else []
        self.depth = depth
        self._instance_cache = {}

    def __setattr__(self, name, value):
        if hasattr(self, 'frozen') and self.frozen:
            raise Exception('Frozen node.')
        else:
            super(Node, self).__setattr__(name, value)

    def freeze(self):
        map(lambda n: n.freeze(), self.children)
        self.frozen = True

    def clone(self, parent=None, depth=0):
        children = [c.clone(parent=self, depth=depth+1) for c in self.children]
        clsname = utils.R2C(self.node.expr.name)
        nodeclass = _cache[clsname] if clsname!='_' else Node
        return nodeclass(parent, self.node, children, depth)

    def query(self, cond, topdown=True, **params):
        # TODO: supports context-aware search such as "declared(mynode)", mynode=names[0]
        def _query(parent, node, depth, bag):
            try:
                newlocals = locals().copy()
                newlocals.update(bag[2])
                if eval(bag[1], {}, newlocals):
                    bag[0].append(node)
            except Exception as err:
                bag[3].append(err)
                return False

        bag = {'_query':([], cond, params, [])}
        if topdown:
            ret = self.topdown_visit(_query=_query, bag=bag)
        else:
            ret = self.bottomup_visit(_query=_query, bag=bag)
        if bag['_query'][3]:
            return []
        else:
            return bag['_query'][0]

    def _visit(self, istopdown, node, parent=None, bag=None, depth=0, **actions):
        out = True
        if bag is None:
            bag = dict((a,[]) for a, m in actions.items() if callable(m))
        if istopdown:
            for action, method in actions.items():
                out = method(parent, node, depth, bag[action])
                if out is False:
                    break
            if out is not False:
                for n in node.children:
                    bag = self._visit(True, n, parent=node, bag=bag, depth=depth+1, **actions)
        else:
            for n in node.children:
                bag = self._visit(False, n, parent=node, bag=bag, depth=depth+1, **actions)
            for action, method in actions.items():
                out = method(parent, node, depth, bag[action])
                if out is False:
                    break
        return bag

    def topdown_visit(self, node=None, bag=None, **actions):
        return self._visit(True, self, bag=bag, **actions)

    def bottomup_visit(self, node=None, bag=None, **actions):
        return self._visit(False, self, bag=bag, **actions)

    def __str__(self):
        return self.tostr()

    def showtree(self):
        def indent(text):
            return '\n'.join(('  ' + line) for line in text.splitlines())
        def nodestr(node):
            text = u'"%s"' % node.node.text if len(node.node.children)==0 else ''
            #return u'%s:%s' % (node.node.expr.name, text)
            return u'%s:%s' % (node.__class__.__name__, text)
        def skipblank(node):
            return node.expr.name in ['_0', '_1', '_L', '_CL', '_B', '_S', 'EOL']
        print(self.tostr(text=nodestr, skip=skipblank, control=indent,
            joinstr='\n'))

    def applymaps(self, text):
        # multiline -> include -> formatmap -> stringmap -> commentmap
        # TODO: remove mapping check
        if self.cmap:
            for k, v in self.cmap.items():
                text = text.replace(k, v)
        if self.smap:
            for k, (v, ch) in self.smap.items():
                text = text.replace(k, ch+v+ch)
        if self.fmap:
            for k, v in self.fmap.items():
                text = text.replace(k, v)

        return text

    def srcgen(self, text):
        # multiline -> include -> formatmap -> stringmap -> commentmap
        text = self.applymaps(text)
        # undo include

        # undo multiline
        return text

    def tostr(self, text=None, skip=None, control=None, joinstr='', depth=0):
        if skip and skip(self.node):
            return ''
        if text:
            ret = [text(self)]
        else:
            ret = ['%s' % self.node.text if len(self.node.children)==0 else '']
        for n in self.children:
            if control:
                ret.append(control(n.tostr(text=text, skip=skip, control=control,
                    joinstr=joinstr, depth=depth+1)))
            else:
                ret.append(n.tostr(text=text, skip=skip, control=control,
                    joinstr=joinstr, depth=depth+1))
        outstr = joinstr.join([r for r in ret if r])

        return self.srcgen(outstr) if depth==0 else outstr

def generate_tree(node, parent=None, depth=0, lift_child=True,
     remove_blanknode=True):
    children = [_n for _n in node if not remove_blanknode or _n.start!=_n.end]
    if lift_child and len(children) == 1:
        return generate_tree(children[0], parent=node, depth=depth)
    else:
        if not node.expr.name:
            return Node(parent, node, [generate_tree(child, parent=node,
                depth=depth+1, lift_child=lift_child, remove_blanknode=remove_blanknode)
                for child in children], depth)
        else:
            clsname = utils.R2C(node.expr.name)
            if clsname in _cache:
                nodeclass = _cache[clsname]
            else:
                nodeclass = type(str(clsname), (Node,),{})
                _cache[clsname] = nodeclass
            return nodeclass(parent, node, [generate_tree(child, parent=node,
                depth=depth+1, lift_child=lift_child, remove_blanknode=remove_blanknode)
                for child in children], depth)

class Grammar(pGrammar):

    def parse(self, prep, pos=0, lift_child=True, remove_blanknode=True,
        apply_stringmap=True, apply_commentmap=True):
        parse_tree = super(Grammar, self).parse('\n'.join(prep['newlines']),
            pos=pos)
        tree = generate_tree(parse_tree, lift_child=lift_child,
            remove_blanknode=remove_blanknode)
        if apply_stringmap:
            tree.smap = prep['stringmap']
        if apply_commentmap:
            tree.cmap = prep['commentmap']
        tree.fmap = prep['formatmap']
        tree.freeze()
        return tree

