from sympy.core.alphabets import greeks
from sympy.printing.conventions import split_super_sub
from sympy.printing.ccode import C99CodePrinter

translation = ('A', 'B', 'G', 'D', 'E', 'Z', 'H', 'Th', 'I', 'K', 'L', 'M', 'N', 'X', 'O', 'P', 'R', 'S', 'T', 'Y', 'Ph', 'Kh', 'Ps', 'W')
greek_to_latin = dict(zip(greeks, translation))

class GnuplotPrinter(C99CodePrinter):
    """Print to gnuplot with simplified names"""
    def _print_Symbol(self, expr):
        name, sup, sub = split_super_sub(expr.name) 
        return greek_to_latin.get(name, name)

gplot = GnuplotPrinter().doprint
