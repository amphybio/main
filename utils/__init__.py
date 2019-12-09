#!/usr/bin/env python3
# vim: fileencoding=utf-8
# authors: Leonardo Gama (03/10/2019)

"""
Gerenal programming utilities.
"""

__all__ = ['decorator_with_options', 'memoized', 'plot_points']

import atexit
import diskcache
import hashlib
import inspect
import logging
import numpy as np
import os
from math import ceil, log2, log10
from functools import partial, wraps
from collections.abc import Sequence

try:
    from pip._internal.utils.appdirs import user_cache_dir
except ImportError:
    from pip.utils.appdirs import user_cache_dir


def decorator_with_options(decorator):
    """Make a decorator usable with or without arguments.

    As an example, a decorator created like this:

        @decorator_with_options
        def my_decorator(func, *, opt1=None, opt2=None):
            @functools.wraps(func)
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
    objects, plus dictionaries, to an unequivocal and hashable form.

    Be f this function, normalize numeric and list-like types in such way that:
        f(1) == f(1.0) == f(1+0j)
        f([1, 2]) == f((1, 2))
        f(1) == f([1])

    As a consequence, it converts sympy and numpy numbers and arrays to native
    Python types.

    :obj: any object
    :returns: (tuple of [tuples of]) objects, with numbers coerced to complex
    """
    if isinstance(obj, (bool, str)):
        return obj
    if isinstance(obj, dict):
        return {_normalize_type(k): _normalize_type(v) for k, v in sorted(obj.items())}
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
def memoized(func, *, size_limit=10**8, eviction_policy='least-recently-used', cache_dir=CACHE_DIR,
             typed=False, ignore_args=None):
    """Persistent memoization function decorator with argument normalization and ignore list.

    :func: a callable object that is not a method
    :size_limit: (int, in bytes) approximate size limit of cache - default 100 MB
    :eviction_policy: rule to evict cache if size_limit is reached, any of
        diskcache.EVICTION_POLICY
    :typed: wheter to consider lists of identically valued arguments of different types as
        different arguments lists
    :cache_dir: location (directory path) of persistent cache files
    :ignore_args: name or list of names of parameters to ignore
    :returns: a memoized version of function 'func'
    """
    func_id = "{}.{:0>4s}".format(func.__qualname__, hashlib.md5(func.__code__.co_code).hexdigest()[-4:])
    cache_dir = os.path.join(cache_dir, func_id)
    func.cache = diskcache.Cache(cache_dir, size_limit=size_limit, eviction_policy=eviction_policy)
    atexit.register(func.cache.close)

    arg_names = inspect.getfullargspec(func).args
    func.ignore_args = frozenset(ignore_args) if ignore_args else None

    @wraps(func)
    def wrapper(*args, **kwargs):
        key = kwargs.copy()
        key.update(zip(arg_names, args))
        if ignore_args is not None:
            key = {k: v for k, v in key.items() if k not in ignore_args}
        if not typed:
            key = {k: _normalize_type(v) for k, v in key.items()}
        key = dict(sorted(key.items()))

        try:
            return func.cache[key]
        except KeyError:
            logging.debug("Cache miss on key %s", repr(key))
            val = func(*args, **kwargs)
            func.cache[key] = val
            return val

    return wrapper


def plot_points(xmin, xmax, min_points, logspace=False):
    """Generate stable points in range [xmin:xmax]"""
    if xmin < 0:
        raise ValueError("xmin must be >= 0")

    range_func = np.logspace if logspace else np.linspace

    if logspace:
        xmin += 1  # shift by one
        xmax += 1
        xmin, xmax = log10(xmin), log10(xmax)
    bound = 2**ceil(log2(xmax))
    # discount the 3 extra points: xmin, xmax and 1 added to 2**n
    min_points = min_points*bound/(xmax - xmin) - 3
    n_points = 2**ceil(log2(min_points)) + 1
    points = range_func(0, bound, n_points)
    if logspace:
        xmin, xmax = 10**xmin, 10**xmax
    points = [x for x in points if xmin < x < xmax]
    points.insert(0, xmin)
    points.append(xmax)
    points = np.array(points)
    if logspace:
        points -= 1  # shift back
    return points
