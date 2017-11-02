from context import fasin
from fasin import cpp
import unittest, sys, os, shutil

here = os.path.dirname(os.path.realpath(__file__))
prog = os.path.join(here, 'cpp1.c')

class TestParser(unittest.TestCase):

    def test_cpp(self):
        cprep = cpp(prog)
        return preprocessed

if __name__ == '__main__':
    unittest.main.main()
