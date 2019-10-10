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
