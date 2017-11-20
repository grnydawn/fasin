from context import fasin
from fasin import prep
import unittest, sys, os, shutil

here = os.path.dirname(os.path.realpath(__file__))
prog = os.path.join(here, 'add0.f90')

class TestParser(unittest.TestCase):

    def test_cpp(self):
        fprep = prep(prog)
        print(fprep)

if __name__ == '__main__':
    unittest.main.main()
