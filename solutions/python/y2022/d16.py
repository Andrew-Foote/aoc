from dataclasses import dataclass
import functools as ft
import re
from solutions.python.lib import graph

test_inputs = [('example', '''\
Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II\
''', [
	('p1', '1651'),
	('p2', '0')
])]

TICKS_TILL_ERUPTION = 30

@dataclass
class Valve:
	name: str
	flow_rate: int
	tunnels: list[str]

def parse(ip: str) -> dict[str, Valve]:
	r = {}

	for line in ip.splitlines():
		name, flow_rate, tunnels_s = re.match(
			r'Valve ([A-Z]{2}) has flow rate=(\d+); '
			r'tunnels? leads? to valves? ([A-Z]{2}(?:, [A-Z]{2})*)',
			line
		).groups()

		tunnels = tunnels_s.split(', ')
		r[name] = Valve(name, int(flow_rate), tunnels)

	return r

def p1(ip: str) -> int:
	valves = parse(ip)
	valve_children = lambda valve_name: valves[valve_name].tunnels

	# There's no point moving to a valve if all of its descendants (including itself) are already
	# open. We cache the set of descendants for each valve up-front in order to allow us to check
	# this quickly during the search.
	#
	# (This optimization doesn't really make much of a difference, but I thought of it before I
	# thought of checking for whether the flow rate of the valve to be opened is zero, and I guess
	# it's not doing any harm.)

	valve_descendants = {
		valve_name: frozenset(graph.dfs(valve_name, valve_children))
		for valve_name in valves.keys()
	}

	# We have to cache this function to make it work, otherwise it spends too much time repeating
	# calculations.
	@ft.cache
	def maximal_additional_pressure_yield(
		ticks: int, cur_valve_i: str, open_valves: frozenset[str]
	) -> int:

		"""Returns the maximum additional pressure we can release (besides what we are already
		releasing each tick) during the remaining ticks."""

		# If we do nothing, we will release 0 additional pressure.
		m = 0

		# If we don't have any time left, there's nothing we can do.
		if ticks < TICKS_TILL_ERUPTION:

			# If the current valve isn't already open, we can open i. This is pointless if the flow
			# rate is 0, however.

			if cur_valve_i not in open_valves and valves[cur_valve_i].flow_rate:
				open_returns = (
					valves[cur_valve_i].flow_rate * (TICKS_TILL_ERUPTION - (ticks + 1))
					+ maximal_additional_pressure_yield(ticks + 1, cur_valve_i, open_valves | {cur_valve_i})
				)

				if open_returns > m:
					m = open_returns

			# We could also try moving to another valve. As noted above, this is pointless if the
			# valve's descendants (including itself) are already open.

			for valve_i in valves[cur_valve_i].tunnels:
				if valve_descendants[valve_i].issubset(open_valves):
					continue

				move_returns = maximal_additional_pressure_yield(ticks + 1, valve_i, open_valves)

				if move_returns > m:
					m = move_returns

		return m

	return maximal_additional_pressure_yield(0, 'AA', frozenset())

def p2(ip: str) -> int:
	return 0