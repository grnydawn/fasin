from context import fasin
from fasin import main, prep
import unittest, sys, os, shutil

here = os.path.dirname(os.path.realpath(__file__))
prog = os.path.join(here, 'add1.f90')

class TestParser(unittest.TestCase):

    def test_prep(self):
        prep(prog)

    def _test_parse(self):
        main(prog)
        #print(sys.stdout.getvalue())
        #import pdb; pdb.set_trace()
        #print(sys.stdout)

if __name__ == '__main__':
    unittest.main.main()
