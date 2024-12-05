from contextlib import chdir
from dataclasses import dataclass
import functools as ft
import json
from pathlib import Path
import subprocess

import methodlib

@ft.cache
def project_path() -> Path:
    return Path(f'solutions/haskell')

@dataclass(frozen=True)
class HasFacetMode:
    facet: str

@dataclass(frozen=True)
class RunFacetMode:
    facet: str
    ip: str

@dataclass(frozen=True)
class TestDefsMode:
    pass

Mode = HasFacetMode | RunFacetMode | TestDefsMode

@ft.cache
def _compile() -> None:
    subprocess.run(['cabal', 'build'])

def _run(year: int, day: int, mode: Mode) -> str:
    args: tuple[str, ...]
    ip: str | None

    match mode:
        case HasFacetMode(facet):
            args = 'has_facet', facet
            ip = None
        case RunFacetMode(facet, ip):
            args = 'run_facet', facet
        case TestDefsMode():
            args = 'test_defs',
            ip = None

    with chdir(project_path()):
        _compile()

        return subprocess.check_output([
            'cabal', 'run', 'aoc', '--', str(year), str(day), *args, 
        ], encoding='utf-8', input=ip)

def has_facet(year: int, day: int, facet: str) -> bool:
    output = _run(year, day, HasFacetMode(facet)).strip()

    match output:
        case 'y':
            return True
        case 'n':
            return False
        case _:
            raise RuntimeError(
                f"expected 'y' or 'n' as 'has_facet' output, got '{output}'"
            )

def run_facet(year: int, day: int, facet: str, ip: str) -> str:
    return _run(year, day, RunFacetMode(facet, ip)).strip()

def test_defs(year: int, day: int) -> methodlib.TestDefs:
    output = _run(year, day, TestDefsMode())
    decoded = json.loads(output)
    result: methodlib.TestDefs = []

    for name, ip, facets in decoded:
        result.append((name, ip, [
            (facet[0], facet[1]) for facet in facets
        ]))

    return result

methodlib.register('haskell', has_facet, run_facet, test_defs)