from .fortparser import parse
#from .fortprep import prep
from .cpp99 import cpp
from .cfim import span, restore
from .xform import prep
from .utils import CMAPSTR, SMAPSTR

banner = '''
###########################################
# Python Fortran 2003 Parser and Analyzer #
###########################################
'''

def ishell():
    import readline
    import code
    import os
    def load(path, lift_child=False, remove_blanknode=True,
        apply_stringmap=True, apply_commentmap=True):

        return parse(prep(os.path.expanduser(path)), lift_child=lift_child,
            remove_blanknode=remove_blanknode,
            apply_stringmap=apply_stringmap,
            apply_commentmap=apply_commentmap)

    variables = {'load': load}
    shell = code.InteractiveConsole(variables)
    shell.interact(banner=banner)
