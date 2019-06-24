# vim: fileencoding=utf-8
# authors: Leonardo Gama (10/06/2019)

"""
Special mathematical functions.
"""

from sympy import Function, hyper, prod
from sympy.printing.pretty.stringpict import prettyForm

from .printing import subscript


class KummerM(hyper):
    """
    KummerM function with pretty printing capabilities, based on SymPy's hypergeometric function.
    TODO: evaluation not working...
    """
    def __new__(cls, a, b, z):
        return super().__new__(cls, (a,), (b,), z)
    def _pretty(self, printer):
        p = printer.doprint
        return prettyForm("M({}, {}, {})".format(p(self.args[0][0]), p(self.args[1][0]), p(self.args[2])))
    def _latex(self, printer):
        p = printer.doprint
        return r"\mathcal{{M}}\left({},\ {},\ {}\right)".format(p(self.args[0][0]), p(self.args[1][0]), p(self.args[2]))

# Temporary monkey patch:
KummerM = lambda a, b, z: hyper((a,), (b,), z)


class pochhammer(Function):
    """
    Pochhammer symbol, raising factorial.

    pochhammer(x, n) = x⋅(x+1)⋅(x+2)⋅⋅⋅(x+n-1)
    """
    @classmethod
    def eval(cls, x, n):
        if n.is_integer:
            return prod((x + i for i in range(n)))
    def _pretty(self, printer):
        p = printer.doprint
        return prettyForm("({}){}".format(p(self.args[0]), subscript(p(self.args[1]))))
    def _latex(self, printer):
        p = printer.doprint
        return r"\left({}\right)_{{{}}}".format(p(self.args[0]), p(self.args[1]))
