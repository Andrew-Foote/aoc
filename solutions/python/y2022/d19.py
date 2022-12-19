from collections import Counter, defaultdict
from collections.abc import Iterator
from dataclasses import dataclass
from enum import Enum
import functools as ft
import re
from typing import Self, Type
from solutions.python.lib import graph

test_inputs = [('example', '''\
Blueprint 1:
  Each ore robot costs 4 ore.
  Each clay robot costs 2 ore.
  Each obsidian robot costs 3 ore and 14 clay.
  Each geode robot costs 2 ore and 7 obsidian.

Blueprint 2:
  Each ore robot costs 2 ore.
  Each clay robot costs 3 ore.
  Each obsidian robot costs 3 ore and 8 clay.
  Each geode robot costs 3 ore and 12 obsidian.\
''', [
    ('quality_levels_s', '9,24'),
    ('p1', 33)
])]

class Resource(Enum):
    ORE = 'ore'
    CLAY = 'clay'
    OBSIDIAN = 'obsidian'
    GEODE = 'geode'

RESOURCE_PRIORITY = (Resource.GEODE, Resource.OBSIDIAN, Resource.CLAY, Resource.ORE)

Blueprint = defaultdict[Resource, Counter[Resource]]

def parse(ip: str) -> dict[int, Blueprint]:
    blueprints = {}

    for blueprint_s in ip.split('\n\n'):
        m = re.match(r'Blueprint (\d+):', blueprint_s)
        assert m is not None
        blueprint_id, = m.groups()
        blueprint_id = int(blueprint_id)
        blueprint_s = blueprint_s[m.end():]

        lines = [line.strip() for line in blueprint_s.split('.') if line.strip()]
        blueprint = defaultdict(lambda: Counter())

        for line in lines:
            m = re.match(r'Each (\w+) robot costs (\d+) (\w+)(?: and (\d+) (\w+))?', line)
            assert m is not None
            r, c1, cr1, c2, cr2 = m.groups()
            robot_cost = blueprint[Resource(r)]
            robot_cost[Resource(cr1)] = int(c1)
            if c2: robot_cost[Resource(cr2)] = int(c2)

        blueprints[blueprint_id] = blueprint

    return blueprints

# the nodes in our graph will be robot-minute pairs
# 
# e.g. considering blueprint 1 from the example:
# the start node (not associated with any particular robot or minute)
# goes to:
#   ore robot on minute 4
#   clay robot on minute 2
#   (obsidian not immediately available)
#   (geode not immediately available)

@dataclass
class State:
    minute: int
    robots: Counter[Resource]
    stash: Counter[Resource]

    @classmethod
    def initial(cls: Type[Self]) -> Self:
        return cls(0, Counter({Resource.ORE: 1}), Counter())

    def next_states(self: Self, max_minutes: int, blueprint: Blueprint) -> Iterator[Self]:
        if self.minute < max_minutes:
            for resource in Resource:
                cost = blueprint[resource]
                buildable = self.stash >= cost

                if buildable and (max_minutes - (self.minute + 1)) > cost[resource]:
                    yield self.__class__(
                        self.minute + 1,
                        self.robots + Counter({resource: 1}),
                        (self.stash - cost) + self.robots
                    )

            yield self.__class__(self.minute + 1, self.robots, self.stash + self.robots)

    def benefit_lower_bounds(self: Self, max_minutes: int) -> Counter[Resource]:
        return Counter({
            resource: self.stash[resource] + self.robots[resource] * (max_minutes - self.minute)
            for resource in Resource
        })

    @ft.total_ordering
    def __lt__(self: Self, other: Self) -> bool:
        slbs = self.benefit_lower_bounds(24)
        olbs = other.benefit_lower_bounds(24)
        slbso = tuple(slbs[resource] for resource in RESOURCE_PRIORITY)
        olbso = tuple(olbs[resource] for resource in RESOURCE_PRIORITY)
        return slbso <= olbso

    # def eventual_benefit_lower_bound(self: Self, max_minutes: int) -> int:
    #     return self.stash[Resource.GEODE] + self.robots[Resource.GEODE] * (max_minutes - self.minute)

def max_geodes(max_minutes: int, blueprint: Blueprint) -> int:
    res = 0

    print()
    print('Searching with blueprint {}'.format({ r1.value: {r2.value: cost for r2, cost in costs.items()} for r1, costs in blueprint.items() }))
    input()

    for state in graph.dfs(
        State.initial(),
        lambda state: state.next_states(max_minutes, blueprint)
    ):
        if state.minute == 24:
            print(state)
            geodes = state.stash[Resource.GEODE]

            if geodes > res:
                res = geodes

    return res

def quality_levels(ip: str) -> Iterator[int]:
    blueprints = parse(ip)

    for id_, blueprint in blueprints.items():
        geodes = max_geodes(24, blueprint)
        yield id_ * geodes

def quality_levels_s(ip: str) -> str:
    return ','.join(map(str, quality_levels(ip)))

def p1(ip: str) -> int:
    return sum(quality_levels(ip))

def p2(ip: str) -> int:
    return 0