# vim: fileencoding=utf-8
# authors: Leonardo Gama (29/05/2019)

"""
Custom printers for SymPy's pretty-printing system.
"""

from sympy.core.alphabets import greeks
from sympy.printing.ccode import C99CodePrinter
# from sympy.printing.codeprinter import CodePrinter
from sympy.printing.conventions import split_super_sub
from sympy.printing.str import StrPrinter


# Small greek letters to upper case latin letter(s) dictionary
latin = ('A','B','G','D','E','Z','H','Th','I','K','L','M','N','X','O','P','R','S','T','Y','Ph','Kh','Ps','W')
greek_to_latin = dict(zip(greeks, latin))

sub_table = str.maketrans('aehijklmnoprstuvx0123456789+-=() ', 'ₐₑₕᵢⱼₖₗₘₙₒₚᵣₛₜᵤᵥₓ₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎ ')

def subscript(text):
    """
    Convert text to Unicode subscript if possible ―some letters have no subscript counterpart.
    """
    if all(ord(c) in sub_table for c in text):
        return text.translate(sub_table)
    elif ' ' in text:
        return "_({})".format(text)
    else:
        return "_" + text


class GnuplotPrinter(C99CodePrinter):
    """
    Print to gnuplot expression with simplified names.
    """
    def _print_Symbol(self, expr):
        name, sup, sub = split_super_sub(expr.name) 
        return greek_to_latin.get(name, name)

gnuplot = GnuplotPrinter().doprint


class MaplePrinter(StrPrinter):
    """
    Print to Maple code.
    """
    def _print_Symbol(self, expr):
       name, sup, sub = split_super_sub(expr.name) 
       return '__'.join([name] + sub)

maple = MaplePrinter().doprint


class OfficePrinter(StrPrinter):
    """
    Print to LibreOffice Math markup language.
    """
    def _print_Symbol(self, expr):
        name, sup, sub = split_super_sub(expr.name) 
        if name == 'lamda':
            name = 'lambda'
        if name.lower() in greeks:
            if not name.islower():
                name = name.upper()
            name = '%' + name
        if sub:
            name += '_{%s}' % sub[0]
        return name

office = OfficePrinter().doprint
