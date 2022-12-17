from dataclasses import dataclass
import functools as ft
import itertools as it
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

@dataclass
class State:
	ticks: int
	occupied: tuple[str, ...]
	opened: frozenset[str]



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

def p2_pain(ip: str) -> int:
	from contextlib import redirect_stdout
	import io

	valves = parse(ip)
	valve_children = lambda valve_name: valves[valve_name].tunnels

	valve_descendants = {
		valve_name: frozenset(graph.dfs(valve_name, valve_children))
		for valve_name in valves.keys()
	}

	# can we bound the return value of maximal_additional_pressure_yield above?
	# indeed, we should be able to: the only additional yields we can get are those
	# by opening the remaining values. so if we just add all those flow rate sup,
	# that's certainly an upper bound

	_manual_cache = {}

	def maximal_additional_pressure_yield(
		ticks: int, self_valve: str, ele_valve: str, open_valves: frozenset[str]
	) -> int:
		#print(f'calculating MAPY for {ticks=}, {self_valve=}, {ele_valve=}, {open_valves=}')

		def options_for(cur_valve: str, open_valves: frozenset[str]=open_valves):
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

		if open_valves.issuperset(valves.keys()):
			return 0

		self_options = options_for(self_valve)
		ele_options = options_for(ele_valve)

		if not self_options:
			if not ele_options:
				return 0

		if (ticks, self_valve, ele_valve, open_valves) in _manual_cache:
			return _manual_cache[ticks, self_valve, ele_valve, open_valves]

		#print(f'cache size: {len(_manual_cache)}')

		if self_options and not ele_options:
			ele_options.append((0, ele_valve, frozenset()))

		if ele_options and not self_options:
			self_options.append((0, ele_valve, frozenset()))

		cmax = 0

		for self_immreturn, new_self_valve, new_self_open_valves in self_options:
			for ele_immreturn, new_ele_valve, new_ele_open_valves in ele_options:
				if new_self_open_valves.isdisjoint(new_ele_open_valves):
					new_open_valves = open_valves | new_self_open_valves | new_ele_open_valves

					#print(f'  option: self: {self_valve} -> {new_self_valve}, ele: {ele_valve} -> {new_ele_valve}, new open valves: {",".join(new_open_valves)}')

					#with redirect_stdout(io.StringIO()) as f:
					immbenefit = self_immreturn + ele_immreturn

					# try an upper bound on the benefit, so we can hopefully ignore some candidates
					# without computing the whole additional pressure yield
					benefit_ub = immbenefit + (
						sum(
							valves[vn].flow_rate for vn in valves.keys()
							if vn not in new_open_valves
						) * (TICKS_TILL_ERUPTION - (ticks + 1))
					)

					if immbenefit + benefit_ub > cmax:
						benefit = immbenefit + maximal_additional_pressure_yield(
							ticks + 1, new_self_valve, new_ele_valve,
							open_valves | new_open_valves
						)

						if benefit > cmax:
							cmax = benefit
					# else:
					# 	print('benefit paid off')

					# op = f.getvalue()

					# for line in op.splitlines():
						#print(f'  | {line}')

					#print(f'  calculated benefit: {benefit}')

		# options = [
		# 	self_immreturn + ele_immreturn + maximal_additional_pressure_yield(
		# 		ticks + 1, new_self_valve, new_ele_valve,
		# 		open_valves | new_self_open_valves | new_ele_open_valves
		# 	)
		# 	for self_immreturn, new_self_valve, new_self_open_valves in options_for(self_valve)
		# 	for ele_immreturn, new_ele_valve, new_ele_open_valves in options_for(ele_valve, open_valves | new_self_open_valves)
		# ]

		#print(options)

		_manual_cache[ticks, self_valve, ele_valve, open_valves] = cmax
		return cmax

	stupid_valves = {valve_name for valve_name, valve in valves.items() if valve.flow_rate == 0}
	return maximal_additional_pressure_yield(4, 'AA', 'AA', frozenset(stupid_valves))

def p2(ip: str) -> int:
	# lifting an idea from reddit

	valves = parse(ip)
	valve_children = lambda valve_name: valves[valve_name].tunnels

	valve_descendants = {
		valve_name: frozenset(graph.dfs(valve_name, valve_children))
		for valve_name in valves.keys()
	}

	@ft.cache
	def maximal_additional_pressure_yield(
		ticks: int, cur_valve: str, open_valves: frozenset[str]
	) -> int:
		def options_for(cur_valve: str):
			options = []

			if ticks < TICKS_TILL_ERUPTION - 1:
				if cur_valve not in open_valves: #and valves[cur_valve].flow_rate:
					options.append((
						valves[cur_valve].flow_rate * (TICKS_TILL_ERUPTION - (ticks + 1)),
						cur_valve,
						frozenset({cur_valve})
					))

			if ticks < TICKS_TILL_ERUPTION - 2:
				for valve in valves[cur_valve].tunnels:
					# if valve_descendants[valve].issubset(open_valves):
					# 	continue

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

	valve_set = frozenset(valves.keys())
	zero_valves = frozenset({valve_name for valve_name, valve in valves.items() if valve.flow_rate == 0})	
	nonzero_valves = valve_set - zero_valves
	
	ssets = list(map(frozenset, it.chain.from_iterable(
		it.combinations(nonzero_valves, r) for r in range(len(nonzero_valves) + 1)
	)))

	therealmax = 0
	thelen = len(ssets) // 2

	for i, self_set in enumerate(ssets[:thelen]):
		ele_set = nonzero_valves - self_set
		self_max = maximal_additional_pressure_yield(4, 'AA', ele_set | zero_valves)
		ele_max = maximal_additional_pressure_yield(4, 'AA', self_set | zero_valves)
		maximal_additional_pressure_yield.cache_clear()
		themax = self_max + ele_max
		print(f'{i}/{thelen}', themax, ';', self_set, ';', ele_set)

		if themax > therealmax:
			therealmax = themax

	return therealmax