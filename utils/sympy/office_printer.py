from sympy.core.alphabets import greeks
from sympy.printing.conventions import split_super_sub
from sympy.printing.str import StrPrinter

class OfficePrinter(StrPrinter):
    """Print to LibreOffice Math markup language"""
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
