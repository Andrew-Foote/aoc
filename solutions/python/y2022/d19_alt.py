# Alternative approach to day 19 using linear programming.

from collections import Counter, defaultdict
from collections.abc import Iterator
from enum import Enum
import re
from typing import Self, Type
from solutions.python.lib import linopt

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
        blueprint: defaultdict[Resource, Counter[Resource]] = defaultdict(lambda: Counter())

        for line in lines:
            m = re.match(r'Each (\w+) robot costs (\d+) (\w+)(?: and (\d+) (\w+))?', line)
            assert m is not None
            r, c1, cr1, c2, cr2 = m.groups()
            robot_cost = blueprint[Resource(r)]
            robot_cost[Resource(cr1)] = int(c1)
            if c2: robot_cost[Resource(cr2)] = int(c2)

        blueprints[blueprint_id] = blueprint

    return blueprints

def max_geodes(minutes: int, blueprint: Blueprint) -> int:
     # per-minute per-resource variables
    builds: list[dict[Resource, linopt.LinPoly]] = []
    robots: list[dict[Resource, linopt.LinPoly]] = []
    generated: list[dict[Resource, linopt.LinPoly]] = []
    stash: list[dict[Resource, linopt.LinPoly]] = []

    constraints: list[linopt.Constraint] = []

    for minute in range(minutes + 1):
        builds_now: dict[Resource, linopt.LinPoly] = {}
        robots_now: dict[Resource, linopt.LinPoly] = {}
        generated_now: dict[Resource, linopt.LinPoly] = {}
        stash_now: dict[Resource, linopt.LinPoly] = {}

        builds.append(builds_now)
        robots.append(robots_now)
        generated.append(generated_now)
        stash.append(stash_now)

        for i, resource in enumerate(Resource):
            LETTER = {Resource.ORE: 'a', Resource.CLAY: 'b', Resource.OBSIDIAN: 'c', Resource.GEODE: 'd'}
            builds_now[resource] = linopt.var(f'{resource.value}_robots_built({minute})')            
            robots_now[resource] = linopt.const(1 if resource == resource.ORE else 0)

            for t in range(minute):
                robots_now[resource] += builds[t][resource]

        for i, resource in enumerate(Resource):
            generated_now[resource] = robots_now[resource] - sum(
                blueprint[robot_resource][resource] * builds_now[robot_resource]
                for robot_resource in Resource
            )

            stash_now[resource] = linopt.const(0)

            for t in range(minute):
                stash_now[resource] += generated[t][resource]

        for i, resource in enumerate(Resource):
            for cost_resource, cost in blueprint[resource].items():
                constraints.append(linopt.le(cost * builds_now[resource], stash_now[cost_resource]))

        constraints.append(linopt.le(sum(builds_now[resource] for resource in Resource), 1))

    constraints.extend(linopt.ge(builds[minute][resource], 0) for minute in range(minutes) for resource in Resource)
    constraints.extend(linopt.le(builds[minute][resource], 1) for minute in range(minutes) for resource in Resource)
    constraints.extend(linopt.ge(builds[minutes][resource], 0) for resource in Resource)
    constraints.extend(linopt.le(builds[minutes][resource], 0) for resource in Resource)
    objective = -stash[minutes][Resource.GEODE]
    x, res = linopt.solve(objective, constraints)
    #breakpoint()
    return -round(res)

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