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
import pickle
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


def _normalize_type(obj, round_digits=15):
    """Convert a sequence (of sequences) of numeric and other hashable
    objects, plus dictionaries, to a canonical form.

    Be f this function, normalize numeric and list-like types in such way that:
        f(1) == f(1.0) == f(0.9999999999999999) == f(1+0j) != f(True)
        f([1, 2]) == f((1, 2))
        f(1) == f([1])

    As a consequence, it converts sympy and numpy numbers and arrays to native
    Python types.  Numbers are rounded at the least significant floating-point
    decimal digit to avoid cache misses due to imprecision in floats generated
    by functions like range and np.linspace.

    :obj: any object
    :round_digits: number of digits to round to, pass False to disable rounding
    :returns: 'obj' with inner elements coerced (numeric -> complex, sequence -> tuple)
    """
    if isinstance(obj, (bool, str)):
        return obj
    if isinstance(obj, dict):
        return tuple((_normalize_type(k), _normalize_type(v)) for k, v in obj.items())
    elif isinstance(obj, Sequence) or isinstance(obj, np.ndarray) and obj.ndim == 1:
        if len(obj) == 1:
            return _normalize_type(next(iter(obj)))
        else:
            return tuple(_normalize_type(o) for o in obj)
    else:
        try:
            num = complex(obj)
            if not round_digits is False:
                num = complex(round(num.real, round_digits), round(num.imag, round_digits))
            return num
        except TypeError:
            return obj


# Memoization decorator.
CACHE_DIR = user_cache_dir('amphybio')
os.makedirs(CACHE_DIR, exist_ok=True)

@decorator_with_options
def memoized(func, *, size_limit=10**8, eviction_policy='least-recently-used', cache_dir=CACHE_DIR,
             typed=False, round_digits=15, ignore_args=None):
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
            key = {k: _normalize_type(v, round_digits) for k, v in key.items()}
        key = dict(sorted(key.items()))

        try:
            return func.cache[key]
        except KeyError:
            logging.debug("Cache miss on key %s", repr(key))
            val = func(*args, **kwargs)
            func.cache[key] = val
            return val

    return wrapper

def dump_cache(func, file_path='/tmp/{__qualname__}.cache.pkl'):
    file_path = file_path.format(__qualname__=func.__qualname__)
    cache = [(k, func.cache[k]) for k in func.cache]
    with open(file_path, mode='wb') as file:
        pickle.dump(cache, file)

def load_cache(func, file_path='/tmp/{__qualname__}.cache.pkl'):
    file_path = file_path.format(__qualname__=func.__qualname__)
    with open(file_path, mode='rb') as file:
        cache = pickle.load(file)
    for key, value in cache:
        func.cache[_normalize_type(key)] = value


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
