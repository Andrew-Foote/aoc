import functools as ft
import importlib
import methodlib
import mypy.api
from typing import Any

def _module_path(year, day):
    alt = '_alt' if year == 2022 and day == 19 else ''
    return f'solutions/python/y{year}/d{day}{alt}.py'

@ft.cache
def _typecheck(year, day) -> None:
    mypy_stdout, mypy_stderr, mypy_ret = mypy.api.run([_module_path(year, day)])

    if mypy_ret != 0:
        print(f'\n\n{mypy_stdout}')

        input(
            'Typecheck failed; press Ctrl+C to abort, or press enter to continue anyway.'
        )

@ft.cache
def _module(year, day):
    alt = '_alt' if year == 2022 and day == 19 else ''
    return importlib.import_module(f'solutions.python.y{year}.d{day}{alt}')

def has_facet(year: int, day: int, facet: str):
    return hasattr(_module(year, day), facet)

def run_facet(year: int, day: int, facet: str, ip: str) -> str:
    _typecheck(year, day)
    return str(getattr(_module(year, day), facet)(ip))

def test_defs(year: int, day: int) -> methodlib.TestDefs:
    module = _module(year, day)

    try:
        return getattr(module, 'test_inputs')
    except AttributeError:
        return []

methodlib.register('python', has_facet, run_facet, test_defs)