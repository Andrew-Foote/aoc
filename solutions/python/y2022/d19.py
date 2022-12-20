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
    ('p1', 33),
    ('max_geodes_s', '56,62')
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

    blueprint_blocks = ip.split('\n\n')
    if len(blueprint_blocks) == 1: blueprint_blocks = ip.splitlines()

    for blueprint_s in blueprint_blocks:
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

@dataclass
class State:
    minute: int
    robots: Counter[Resource]
    stash: Counter[Resource]

    @classmethod
    def initial(cls: Type[Self]) -> Self:
        return cls(0, Counter({Resource.ORE: 1}), Counter())

    def wait(self: Self, minutes: int) -> Self:
        dstash = Counter({resource: minutes * count for resource, count in self.robots.items()})
        return self.__class__(self.minute + minutes, self.robots, self.stash + dstash)

    def build(self: Self, blueprint: Blueprint, resource: Resource) -> Self:
        cost = blueprint[resource]

        if not self.stash >= cost:
            raise ValueError(f'{resource.value} not buildable: {self}')

        return self.__class__(
            self.minute + 1,
            self.robots + Counter({resource: 1}),
            self.stash - cost + self.robots
        )

    def next_build_states(self: Self, max_minutes: int, blueprint: Blueprint) -> Iterator[Self]:
        any_buildable = False

        for resource in reversed(Resource):
            # do we need to build any more of this robot?
            if resource != Resource.GEODE:
                max_robots_required = max(cost[resource] for cost in blueprint.values())
                if self.robots[resource] >= max_robots_required: continue

            cost = blueprint[resource]
            #print(f'considering resource {resource.value}, which costs: {cost}')
            robots_available = all(self.robots[cost_resource] >= 1 for cost_resource in cost)

            if robots_available:
                remaining_cost = cost - self.stash
                #print(f'can build right now; remaining costs would be: {remaining_cost}')

                # how many turns do we need to wait to satisfy this remaining cost?
                minutes_required_per_resource = Counter({r: 0 for r in Resource})

                for cost_resource, remaining_cost_for_this_resource in remaining_cost.items():
                    #print(f'considering how to meet remaining cost of {remaining_cost_for_this_resource} for resource {cost_resource.value}')
                    robot_count = self.robots[cost_resource]
                    #print(f'we already have {robot_count} robots for this resource')
                    q, r = divmod(remaining_cost_for_this_resource, robot_count)
                    minutes_required_per_resource[cost_resource] = q + 1 if r else q
                    #print(f'so it would take {minutes_required_per_resource[cost_resource]} minutes')

                minutes_required = max(minutes_required_per_resource.values())

                if self.minute + minutes_required <= max_minutes - 1:
                    any_buildable = True
                    yield self.wait(minutes_required).build(blueprint, resource)
                    #print(f'ok we should build a {resource.value} robot')
            else:
                pass
                #print('robots for these resources not built yet')

        if not any_buildable:
            if self.minute < 24:
                yield self.wait(24 - self.minute)

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
        return slbso > olbso

    # def eventual_benefit_lower_bound(self: Self, max_minutes: int) -> int:
    #     return self.stash[Resource.GEODE] + self.robots[Resource.GEODE] * (max_minutes - self.minute)

    def eventual_benefit_upper_bound(self: Self, max_minutes: int) -> int:
        n = max_minutes - self.minute
        return self.stash[Resource.GEODE] + self.robots[Resource.GEODE] * (max_minutes - self.minute) + n * (n + 1) // 2

def max_geodes(max_minutes: int, blueprint: Blueprint) -> int:
    res = 0

    # print()
    # print('Searching with blueprint {}'.format({ r1.value: {r2.value: cost for r2, cost in costs.items()} for r1, costs in blueprint.items() }))
    # input()

    def children(state):
        for s in state.next_build_states(max_minutes, blueprint):
            if s.eventual_benefit_upper_bound(max_minutes) > res:
                yield s

    for i, state in enumerate(graph.dfs(
        State.initial(),
        children
        #lambda state: state.next_build_states(max_minutes, blueprint)
    )):

        if state.minute >= 24:
            # print(state)
            geodes = state.stash[Resource.GEODE]

            if geodes > res:
                res = geodes

            # print(res, i)

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

def max_geodes_s(ip: str) -> str:
    blueprints = parse(ip)
    return ','.join(str(max_geodes(32, blueprint)) for blueprint in blueprints.values())

def p2(ip: str) -> int:
    blueprints = parse(ip)
    return max_geodes(32, blueprints[1]) * max_geodes(32, blueprints[2]) * max_geodes(32, blueprints[3])