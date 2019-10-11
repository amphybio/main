#!/usr/bin/env python3
# vim: fileencoding=utf-8
# authors: Leonardo Gama (03/10/2019)

"""
Gerenal programming utilities.
"""

__all__ = ['decorator_with_options', 'memoize']

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
    if isinstance(obj, Container):
        return tuple(_normalize_type(o) for o in obj)
    else:
        try:
            return complex(obj)
        except:
            return obj

HASH_MASK = 2**16 - 1  # hash length in hexadecimal will be n/4
def memoize(func, *, loc=CACHE_DIR, match_type=True, ignore=None):
    """Persistent memoization function decorators.

    :returns: TODO

    """
    func.id = "{}.{:04x}".format(func.__qualname__, hash(func.__code__.co_code) & HASH_MASK)
    func.cache_path = os.path.join(loc, func_id)
    func.match_type = match_type
    #TODO: need to set as attribute? or wrapped functions can access local variables of wrapper?
    arg_names = inspect.getfullargspec(func).args
    if ignore is not None:
        ignore = {ignore} if isinstance(ignore, str) else set(ignore)

    def wrapper(*args, **kwargs):
        kwargs.update(zip(arg_names, args))

        if ignore is not None:
            args = [args[arg_names.index(a)] for a in arg_names if a not in ignore]
            kwargs = {k: v for k, v in kwargs.items() if k not in ignore}
        if not func.match_type:
            args = _normalize_type(args)
            kwargs = {k: _normalize_type(v) for k, v in kwargs.items()}
        key = repr(args) + repr(sorted((k, v) for k, v in kwargs.items()))

        with shelve.open(func.cache_path) as db:
            try:
                return db[key]
            except KeyError:
                val = func(*args, **kwargs)
                db[key] = val
                return val

    return wrapper
