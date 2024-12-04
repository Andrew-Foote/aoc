import json
from pathlib import Path
import subprocess

import methodlib

@ft.cache
def src_path(year: int, day: int) -> Path:
    return Path(f'solutions/haskell/{year}/D{day}')

@ft.cache
def _compile(year: int, day: int) -> None:
    subprocess.run(['ghc', str(src_path())])

def has_facet(year: int, day: int, facet: str) -> bool:
    return bool(subprocess.check_output([str(src_path()), '-e', facet]))

def run_facet(year: int, day: int, facet: str, ip: str) -> str:
    _compile()
    
    return subprocess.check_output(
        [str(src_path()), '-f', facet], input=ip, encoding='utf-8'
    )

def test_defs(year: int, day: int) -> methodlib.TestDefs:
    output = subprocess.check_output(
        [str(src_path()), '-t'], encoding='utf-8'
    )

    decoded = json.loads(output)
    result: methodlib.TestDefs = []

    for name, ip, facets in decoded:
        result.append((name, ip, [
            (facet_name, facet_expectation) for facet in facets
        ]))

    return result

methodlib.register('haskell', has_facet, run_facet)