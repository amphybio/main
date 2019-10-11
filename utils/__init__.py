#!/usr/bin/env python3
# vim: fileencoding=utf-8
# authors: Leonardo Gama (03/10/2019)

"""
Gerenal programming utilities.
"""

__all__ = ['decorator_with_options', 'memoized']

import hashlib
import inspect
import os
import shelve
from functools import partial, wraps
from collections.abc import Sequence

try:
    import pip._internal as pip
except:
    import pip
from pip.utils.appdirs import user_cache_dir


def decorator_with_options(decorator):
    """Make a decorator usable with or without arguments.

    As an example, a decorator created like:

        @decorator_with_options
        def my_decorator(func, *, opt1=None, opt2=None):
            @wraps(func)
            def wrapper(*args, **kwargs):
                # do stuff with func, opt1, opt2...
            return wrapper

    can be used in any of the following forms.

        @my_decorator
        def f():
            pass

        @my_decorator()
        def f():
            pass

        @my_decorator(opt1='something')
        def f():
            pass

    But this use form without the keyword raises an error, as expected:

        @my_decorator('something')
        def f():
            pass

    Warning: the first optional parameter shall not be a callable.
    """
    @wraps(decorator)
    def wrapper(func=None, *varargs, **kwargs):
        if func is None:
            return partial(decorator, **kwargs)
        elif callable(func):
            return decorator(func, **kwargs)
        else:
            errmsg = "{}() takes 0 positional arguments but {} {} given"
            raise TypeError(errmsg.format(decorator.__name__, 1 + len(varargs), "were" if varargs else "was"))
    return wrapper


def _normalize_type(obj):
    """Convert a sequence (of sequences) of numeric and other hashable
    objects to an unequivocal and hashable form.

    Be f this function, normalize numeric and list-like types in such way that:
        f(1) == f(1.0) == f(1+0j)
        f([1, 2]) == f((1, 2))
        f(1) == f([1])

    As a consequence, it converts sympy and numpy numbers and arrays to native
    Python types.

    :obj: any object
    :returns: (tuple of [tuples of]) objects, with numbers coerced to complex
    """
    if isinstance(obj, str):
        return obj
    elif isinstance(obj, Sequence):
        if len(obj) == 1:
            return _normalize_type(next(iter(obj)))
        else:
            return tuple(_normalize_type(o) for o in obj)
    else:
        try:
            return complex(obj)
        except TypeError:
            return obj


# Memoization decorator.
CACHE_DIR = user_cache_dir('amphybio')
os.makedirs(CACHE_DIR, exist_ok=True)

@decorator_with_options
def memoized(func, *, loc=CACHE_DIR, match_type=True, ignore=None):
    """Persistent memoization function decorator.

    :func: a callable object that is not a method
    :loc: location (directory path) of persistent cache files
    :match_type: wheter to consider lists of identically valued arguments of
        different types as different arguments lists
    :ignore: name or list of names of parameters to ignore in caching mechanism
    :returns: a memoized version of function 'func'

    Warning: doesn't work for multithreaded or multiprocess uses.
    """
    func.id = "{}.{:0>4s}.cache".format(func.__qualname__, hashlib.md5(func.__code__.co_code).hexdigest()[-4:])
    func.cache_path = os.path.join(loc, func.id)

    arg_names = inspect.getfullargspec(func).args
    if ignore is not None:
        ignore = {ignore} if isinstance(ignore, str) else set(ignore)
    func.ignore = ignore

    @wraps(func)
    def wrapper(*args, **kwargs):
        key = kwargs.copy()
        key.update(zip(arg_names, args))
        if ignore is not None:
            key = {k: v for k, v in key.items() if k not in ignore}
        if not match_type:
            key = {k: _normalize_type(v) for k, v in key.items()}
        key = repr(sorted((k, v) for k, v in key.items()))

        try:
            with shelve.open(func.cache_path) as db:
                return db[key]
        except KeyError:
            val = func(*args, **kwargs)
            with shelve.open(func.cache_path) as db:
                db[key] = val
            return val

    return wrapper
