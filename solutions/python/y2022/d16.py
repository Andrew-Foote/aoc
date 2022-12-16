from dataclasses import dataclass
from enum import Enum
import re
from solutions.python.lib import graph
from typing import Iterator, Self

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

class ActionKind(Enum):
	OPEN = 0
	MOVE = 1
	WAIT = 2

@dataclass
class Action:
	kind: ActionKind
	valve_i: int

# To apply Dijkstra's algorithm we need to calculate the cost of moving from one state to another,
# and we need the cost of moving along a path of states to be the sum of those costs. Well, each
# state-move costs us 1 minute of time, multiplied by the opportunity cost of whatever else we
# could be doing, I guess. 

# sum over the final 30-state path = total pressure released over the 30 minutes
# so each edge benefit could be the pressure released during that minute
# i.e. the pressure released by the open valves for that minute

# Dijkstra's algorithm would allow us to find the path through the state-tree that would maximise
# benefit of reaching a state with ticks=30; 

# We have a tree of states, of depth exactly 30. Each state has a tick number (from 0 to 30), a
# current valve index and a set of open valves.
# 
# For each state, we can calculate the 

@dataclass(frozen=True)
class State:
	valves: list[Valve]
	ticks: int
	cur_valve_i: int
	open_valves: set[int]

	def __hash__(self: Self):
		return hash((self.ticks, self.cur_valve_i, tuple(sorted(self.open_valves))))

	def __lt__(self: Self, other: Self) -> bool:
		return self == other

	@property
	def cur_valve(self: Self) -> Valve:
		return self.valves[self.cur_valve_i]

	def ppm(self: Self) -> int:
		return sum(self.valves[valve_i].flow_rate for valve_i in self.open_valves)

	def score(self: Self) -> int:
		# how much pressure will be released over the remaining minutes
		return self.ppm() * (TIME_TILL_ERUPTION - self.ticks)

	def actions(self: Self) -> Iterator[Action]:
		if self.ticks < TIME_TILL_ERUPTION:
			if self.cur_valve_i not in self.open_valves:
				yield Action(ActionKind.OPEN, self.cur_valve_i)

			for valve_i in self.cur_valve.tunnels:
				yield Action(ActionKind.MOVE, valve_i)

			yield Action(ActionKind.WAIT, 0)

	def apply(self: Self, action: Action) -> Self:
		if action.kind == ActionKind.OPEN:
			return State(self.valves, self.ticks + 1, self.cur_valve_i, self.open_valves | {action.valve_i})

		if action.kind == ActionKind.MOVE:
			return State(self.valves, self.ticks + 1, action.valve_i, self.open_valves)

		if action.kind == ActionKind.WAIT:
			return State(self.valves, self.ticks + 1, self.cur_valve_i, self.open_valves)

		assert False

	def next_states(self: Self) -> Iterator[Self]:
		for action in self.actions():
			yield self.apply(action)

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
	# 

	# We need to maximise pressure released
	# At any point in time we are standing at a certain valve
	# Opening the current value takes one minute and releases
	# ressure equal to the valves flow rate per minute after release
	# so the gain from opening the current valve at X minutes is
	#
	#    flow_rate * ( 30 - (X + 1) )
	#
	# Alternatively we could move to the valve with
	#
	# But we could also open up more valves after the current one
	# is opened, so the full formula would be
	#
	#    (flow_rate + GainPerMinute(ChosenNextSteps)) * ( 30 - (X + 1) )

	# Alternatively we can move to a linked valve via a tunnel,
	# which also takes a minute

	# So, we have a finite number of "action paths" and we need to
	# find the one that maximises the presusre released

	# Each action is either "open valve X", "move to valve X",
	# or "nothing" --- though which valves are available to move to / open
	# will depend on the current state, and it will never be useful to do
	# nothng until no actions are left available

	# We are currently standing at valve AA (0)

	valves = parse(ip)
	state = State(valves, 0, 0, set())

	# if we start "backwards"
	# suppose we were at the 29th minute, and still in our initial state.
	# then there is absolutely nothing we could do to release additional pressure, because any action would take a minute to complete.
	# so the only thing to do would be to wait

	# ok, what if we were at the 28th minute? well, now we have two options:
	# - open the current valve... but it has flow rate 0, so that would be useless, instead....
	# - move to a different valve? but that would take a minute, and then it'd take a minute to open it, so it'd still be useless
	# in this situation, the only thing we can do is wait, again
	# however, what if we were at a valve with a positive flow rate?
	# well, then we would have the choice between three options:
	# - open the valve (returns: flow rate * 1 minute)
	# - move to a different valve (returns: 0)
	# - wait (returns: 0)
	# so in this case, opening the valve would be best

	# if we assume we can calculate, for each state, the maximal additional pressure we could yield after reaching that state...
	# 

	# state is a tuple of (ticks, cur_valve_i, open_valves_set)

	import functools as ft
	depth = 0

	@ft.cache
	def maximal_additional_pressure_yield_dumb(state):
		nonlocal depth
		ticks, cur_valve_i, open_valves = state
		# print(' ' * depth + f'{ticks=}, {cur_valve_i=}, {open_valves=}', end='')
		# input()
		# print()

		# waiting will always give us 0 additional pressure beyond what is already "baked in"
		m = 0

		if ticks < TICKS_TILL_ERUPTION:
			# we can only carry out actions if there are ticks left to do

			if cur_valve_i not in open_valves:
				depth += 1
				open_returns = (
					valves[cur_valve_i].flow_rate * (TICKS_TILL_ERUPTION - (ticks + 1))
					+ maximal_additional_pressure_yield(
						(ticks + 1, cur_valve_i, open_valves | {cur_valve_i})
					)
				)
				depth -= 1

				if open_returns > m:
					# print(' ' * depth + f'would be better to open the current valve (valve {cur_valve_i}), giving {open_returns} returns')
					m = open_returns

			for valve_i in valves[cur_valve_i].tunnels:
				depth += 1
				move_returns = maximal_additional_pressure_yield(
					(ticks + 1, valve_i, open_valves)
				)
				depth -= 1

				if move_returns > m:
					# print(' ' * depth + f'would be better to move to valve {valve_i}, giving {move_returns} returns')
					m = move_returns

		# print(' ' * depth + f'best returns: {m}')
		return m

	# return maximal_additional_pressure_yield_dumb((0, 'AA', frozenset()))

	def descendant_valves(valve_i: str) -> frozenset[str]:
		visited = {valve_i}
		stack = [iter(valves[valve_i].tunnels)]

		while stack:
			try:
				cur = next(stack[-1])
			except StopIteration:
				del stack[-1]
			else:
				if cur not in visited:
					visited.add(cur)
					stack.append(iter(valves[cur].tunnels))

		return frozenset(visited)

	cached_descendant_valves = {i: descendant_valves(i) for i in valves.keys()}

	@ft.cache
	def maximal_additional_pressure_yield(ticks: int, cur_valve_i: str, open_valves: frozenset[str]) -> int:
		m = 0

		if ticks < TICKS_TILL_ERUPTION:
			if cur_valve_i not in open_valves and valves[cur_valve_i].flow_rate:
				open_returns = (
					valves[cur_valve_i].flow_rate * (TICKS_TILL_ERUPTION - (ticks + 1))
					+ maximal_additional_pressure_yield(ticks + 1, cur_valve_i, open_valves | {cur_valve_i})
				)

				if open_returns > m:
					m = open_returns

			for valve_i in valves[cur_valve_i].tunnels:
				if valve_i in open_valves and cached_descendant_valves[valve_i].issubset(open_valves):
					continue

				move_returns = maximal_additional_pressure_yield(ticks + 1, valve_i, open_valves)

				if move_returns > m:
					m = move_returns

		return m

	# @ft.cache
	# def maximal_additional_pressure_yield(ticks: int, cur_valve_i: str, open_valves: frozenset[str], dead_valves: frozenset[str]) -> int:
	# 	m = 0

	# 	if ticks < TICKS_TILL_ERUPTION:
	# 		if cur_valve_i not in open_valves:
	# 			new_open_valves = open_valves | {cur_valve_i}
				
	# 			if descendant_valves(cur_valve_i).issubset(new_open_valves):
	# 				new_dead_valves = dead_valves | {cur_valve_i}
	# 			else:
	# 				new_dead_valves = dead_valves

	# 			open_returns = (
	# 				valves[cur_valve_i].flow_rate * (TICKS_TILL_ERUPTION - (ticks + 1))
	# 				+ maximal_additional_pressure_yield(ticks + 1, cur_valve_i, new_open_valves, new_dead_valves)
	# 			)

	# 			if open_returns > m:
	# 				m = open_returns

	# 		for valve_i in valves[cur_valve_i].tunnels:
	# 			if valve_i in dead_valves:
	# 				continue

	# 			move_returns = maximal_additional_pressure_yield(ticks + 1, valve_i, open_valves, dead_valves)

	# 			if move_returns > m:
	# 				m = move_returns

	# 	return m

		# it's pointless to go to any valve which is:
		# (a) already open
		# (b) can only lead to already-open valves
		#     [in fact, it's pointless even if it can lead to a closed valve, but we couldn't get there given the remaining ticks]
		#     [but might not need to go that far with the pruning]

		# so let's go through the open valves and check if they have any descendants that are not-open
		# hm, that's going to be slow
		# instead, easiest thing to do would probably be to precalculate the descendant-sets beforehand?
		# then we can just check, for each open valve, whether its descendant-set is a subset of the open-valve-set


	return maximal_additional_pressure_yield(0, 'AA', frozenset())

	# max_benefit = -1

	# for state, cost in graph.dijkstra(state, lambda state: state.next_states(), lambda state, parent: -state.ppm()):
	# 	benefit = -cost

	# 	if benefit > max_benefit:
	# 		max_benefit = benefit

	# 	print(f'{state=}, {benefit=}, {max_benefit=}')

	# return max_benefit

def p2(ip: str) -> int:
	return 0