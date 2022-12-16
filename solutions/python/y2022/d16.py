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
	('p2', '1707')
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

	# # We have to cache this function to make it work, otherwise it spends too much time repeating
	# # calculations.
	# @ft.cache
	# def maximal_additional_pressure_yield(
	# 	ticks: int, cur_valve: str, open_valves: frozenset[str]
	# ) -> int:

	# 	"""Returns the maximum additional pressure we can release (besides what we are already
	# 	releasing each tick) during the remaining ticks."""

	# 	# We can release at least 0 additional pressure by doing nothing.
	# 	options = [0]

	# 	# Opening a valve takes a minute before it will release any additional pressure, and is
	# 	# therefore pointless if the eruption is only 1 minute away.
	# 	if ticks < TICKS_TILL_ERUPTION - 1:

	# 		# Opening a valve with a flow rate of 0 will not release any additional pressure, and
	# 		# is therefore pointless.
	# 		if cur_valve not in open_valves and valves[cur_valve].flow_rate:
	# 			options.append(
	# 				valves[cur_valve].flow_rate * (TICKS_TILL_ERUPTION - (ticks + 1))
	# 				+ maximal_additional_pressure_yield(
	# 					ticks + 1, cur_valve, open_valves | {cur_valve}
	# 				)
	# 			)

	# 	# Moving to another value will take at least 2 minutes before it allows us to release any
	# 	# additional pressure, and is therefore pointless if the eruption is only 2 minutes away.
	# 	if ticks < TICKS_TILL_ERUPTION - 2:
	# 		for valve in valves[cur_valve].tunnels:

	# 			# Moving to a valve which is already open and whose descendants are all already
	# 			# open will not allow us to open any more valves and is therefore pointless.
	# 			if valve_descendants[valve].issubset(open_valves):
	# 				continue

	# 			options.append(maximal_additional_pressure_yield(ticks + 1, valve, open_valves))

	# 	return max(options)


	@ft.cache
	def maximal_additional_pressure_yield(
		ticks: int, cur_valve: str, open_valves: frozenset[str]
	) -> int:
		def options_for(cur_valve: str):
			options = []

			if ticks < TICKS_TILL_ERUPTION - 1:
				if cur_valve not in open_valves and valves[cur_valve].flow_rate:
					options.append((
						valves[cur_valve].flow_rate * (TICKS_TILL_ERUPTION - (ticks + 1)),
						cur_valve,
						frozenset({cur_valve})
					))

			if ticks < TICKS_TILL_ERUPTION - 2:
				for valve in valves[cur_valve].tunnels:
					if valve_descendants[valve].issubset(open_valves):
						continue

					options.append((0, valve, frozenset()))

			return options

		options = [
			self_immreturn + maximal_additional_pressure_yield(
				ticks + 1, new_self_valve,
				open_valves | new_self_open_valves
			)
			for self_immreturn, new_self_valve, new_self_open_valves in options_for(cur_valve)
		]

		if options:
			return max(options)

		return 0

	return maximal_additional_pressure_yield(0, 'AA', frozenset())

def p2(ip: str) -> int:
	valves = parse(ip)
	valve_children = lambda valve_name: valves[valve_name].tunnels

	valve_descendants = {
		valve_name: frozenset(graph.dfs(valve_name, valve_children))
		for valve_name in valves.keys()
	}

	@ft.cache
	def maximal_additional_pressure_yield(
		ticks: int, self_valve: str, ele_valve: str, open_valves: frozenset[str]
	) -> int:
		def options_for(cur_valve: str):
			options = []

			if ticks < TICKS_TILL_ERUPTION - 1:
				if cur_valve not in open_valves and valves[cur_valve].flow_rate:
					options.append((
						valves[cur_valve].flow_rate * (TICKS_TILL_ERUPTION - (ticks + 1)),
						cur_valve,
						frozenset({cur_valve})
					))

			if ticks < TICKS_TILL_ERUPTION - 2:
				for valve in valves[cur_valve].tunnels:
					if valve_descendants[valve].issubset(open_valves):
						continue

					options.append((0, valve, frozenset()))

			return options

		options = [
			self_immreturn + ele_immreturn + maximal_additional_pressure_yield(
				ticks + 1, new_self_valve, new_ele_valve,
				open_valves | new_self_open_valves | new_ele_open_valves
			)
			for self_immreturn, new_self_valve, new_self_open_valves in options_for(self_valve)
			for ele_immreturn, new_ele_valve, new_ele_open_valves in options_for(ele_valve)
			if new_self_open_valves.isdisjoint(new_ele_open_valves)
		]

		if options:
			return max(options)

		return 0

	return maximal_additional_pressure_yield(4, 'AA', 'AA', frozenset())