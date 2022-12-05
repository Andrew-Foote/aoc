import functools as ft
import importlib
import methodlib
from typing import Any

@ft.cache
def _module(year, day):
    return importlib.import_module(f'solutions.python.y{year}.d{day}')

def has_facet(year: int, day: int, facet: str):
    return hasattr(_module(year, day), facet)

def run_facet(year: int, day: int, facet: str, ip: str) -> str:
    return str(getattr(_module(year, day), facet)(ip))

def test_defs(year: int, day: int) -> methodlib.TestDefs:
    module = _module(year, day)

    try:
        return getattr(module, 'test_inputs')
    except AttributeError:
        return {}

methodlib.register('python', has_facet, run_facet, test_defs)