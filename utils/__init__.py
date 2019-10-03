#!/usr/bin/env python3
# vim: fileencoding=utf-8
# authors: Leonardo Gama (03/10/2019)

import inspect
import os
import shelve
try:
    import pip._internal as pip
except:
    import pip
from pip.utils.appdirs import user_cache_dir


# Memoization decorator.
CACHE_DIR = user_cache_dir('amphybio')
os.makedirs(CACHE_DIR, exist_ok=True)

def _normalize_type(obj):
    if iscontainer(obj):
        return tuple(_normalize_type(o) for o in obj)
    elif isnumeric(obj):
        return float(obj)
    else
        return obj

def memoize(func, *, loc=CACHE_DIR, match_type=True, ignore=None):
    """Persistent memoization function decorators.

    :returns: TODO

    """
    func.id = func.__name__
    func.cache_path = os.path.join(loc, func_id)

    def wrapper(*args, **kwargs):
        args, kwargs = inspect.getfullargspec(func, *args, **kwargs) 
        #ignore...
        if not match_type:
            args = _normalize_type(args)
            kwargs = {k: _normalize_type(v) for k, v in kwargs.items()}
        key = str(args) + str(kwargs)

        with shelve.open(func.cache_path) as db:
            try:
                return db[key]
            except KeyError:
                val = func(*args, **kwargs)
                db[key] = val
                return val

    return wrapper
