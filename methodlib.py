from dataclasses import dataclass
from typing import Callable

HasFacetImpl = Callable[[int, int, str], bool]
RunFacetImpl = Callable[[int, int, str, str], str]

TestDefs = list[tuple[str, str, list[tuple[str, str]]]]
TestDefsImpl = Callable[[int, int], TestDefs]

@dataclass
class Method:
    has_facet: HasFacetImpl
    run_facet: RunFacetImpl
    test_defs: TestDefsImpl = lambda year, day: {}

    def run_part(self, year: int, day: int, part: int, ip: str) -> str:
        return self.run_facet(year, day, f'p{part}', ip)

methods = {}

def register(
    name: str,
    has_facet: HasFacetImpl,
    run_facet: RunFacetImpl,
    test_defs: TestDefsImpl=lambda year, day: {}
):
    methods[name] = Method(has_facet, run_facet, test_defs)