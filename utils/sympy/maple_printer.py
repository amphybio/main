from sympy.printing.conventions import split_super_sub
from sympy.printing.str import StrPrinter
# from sympy.printing.codeprinter import CodePrinter

class MaplePrinter(StrPrinter):
    """Print to Maple code"""

    def _print_Symbol(self, expr):
       name, sup, sub = split_super_sub(expr.name) 
       return '__'.join([name] + sub)

maple = MaplePrinter().doprint
