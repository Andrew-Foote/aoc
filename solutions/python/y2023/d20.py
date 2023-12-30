from collections import Counter, defaultdict, deque
from dataclasses import dataclass
from enum import Enum
from typing import Iterator, Self
from solutions.python.lib.utils import prod
from utils import joinlines

test_inputs = [
	('example', '''\
broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a''',
		[
			('one_press_pulses', '''\
button -low-> broadcaster
broadcaster -low-> a
broadcaster -low-> b
broadcaster -low-> c
a -high-> b
b -high-> c
c -high-> inv
inv -low-> a
a -low-> b
b -low-> c
c -low-> inv
inv -high-> a'''),
			('two_press_pulses', '''\
button -low-> broadcaster
broadcaster -low-> a
broadcaster -low-> b
broadcaster -low-> c
a -high-> b
b -high-> c
c -high-> inv
inv -low-> a
a -low-> b
b -low-> c
c -low-> inv
inv -high-> a'''),
			('pulse_counts_per_one_press_csv', '8,4'),
			('pulse_counts_per_1000_presses_csv', '8000,4000'),
			('p1', '32000000')
		]
	),
	('example2', '''\
broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output''',
		[
			('one_press_pulses', '''\
button -low-> broadcaster
broadcaster -low-> a
a -high-> inv
a -high-> con
inv -low-> b
con -high-> output
b -high-> con
con -low-> output'''),
			('two_press_pulses', '''\
button -low-> broadcaster
broadcaster -low-> a
a -low-> inv
a -low-> con
inv -high-> b
con -high-> output'''),
			('three_press_pulses', '''\
button -low-> broadcaster
broadcaster -low-> a
a -high-> inv
a -high-> con
inv -low-> b
con -low-> output
b -low-> con
con -high-> output'''),
			('four_press_pulses', '''\
button -low-> broadcaster
broadcaster -low-> a
a -low-> inv
a -low-> con
inv -high-> b
con -high-> output'''),
			('five_press_pulses', '''\
button -low-> broadcaster
broadcaster -low-> a
a -high-> inv
a -high-> con
inv -low-> b
con -high-> output
b -high-> con
con -low-> output'''),
			('pulse_counts_per_1000_presses_csv', '4250,2750'),
			('p1', '11687500')
		]
	)
]

class ModuleType(Enum):
	FLIP_FLOP = 0
	CONJUNCTION = 1
	BROADCAST = 3

class SwitchState(Enum):
	OFF = 0
	ON = 1

	@property
	def flipped(self: Self) -> 'SwitchState':
		return SwitchState(1 - self.value)

class Pulse(Enum):
	LOW = 0
	HIGH = 1

	@classmethod
	def from_switch(cls: type[Self], switch: SwitchState) -> 'Pulse':
		return cls(switch.value)

	def __str__(self: Self) -> str:
		return ['low', 'high'][self.value]

@dataclass(frozen=True)
class Module:
	type_: ModuleType
	dsts: list[str]

def parse(ip: str) -> dict[str, Module]:
	mods = {}

	for line in ip.splitlines():
		name, dsts = [p.strip() for p in line.split('->')]
		dsts = [dst.strip() for dst in dsts.split(',')]

		if name[0] == '%':
			name = name[1:]
			type_ = ModuleType.FLIP_FLOP
		elif name[0] == '&':
			name = name[1:]
			type_ = ModuleType.CONJUNCTION
		elif name == 'broadcaster':
			type_ = ModuleType.BROADCAST
		else:
			assert False, name

		mods[name] = Module(type_, dsts)

	return mods

@dataclass
class State:
	flip_flops: dict[str, SwitchState]
	conjunctions: dict[str, dict[str, Pulse]]

	@classmethod
	def initial(cls: type[Self], mods: dict[str, Module]) -> Self:
		flip_flops = {}
		conjunctions = defaultdict(lambda: {})

		for name, mod in mods.items():
			if mod.type_ == ModuleType.FLIP_FLOP:
				flip_flops[name] = SwitchState.OFF

			for dst in mod.dsts:
				if dst in mods and mods[dst].type_ == ModuleType.CONJUNCTION:
					conjunctions[dst][name] = Pulse.LOW

		return cls(flip_flops, conjunctions)

def pulses(mods: dict[str, Module], state: State) -> Iterator[tuple[str, Pulse, str]]:
	queue = deque([('button', Pulse.LOW, 'broadcaster')])
	assert 'broadcaster' in mods

	while queue:
		# print(queue)
		# print(state)
		# input()
		src, pulse, dst = queue.popleft()
		yield src, pulse, dst

		out_pulse = None

		if dst not in mods:
			continue

		match mods[dst].type_:
			case ModuleType.FLIP_FLOP:
				if pulse == Pulse.LOW:
					on_or_off = state.flip_flops[dst]
					state.flip_flops[dst] = on_or_off.flipped
					out_pulse = Pulse.from_switch(state.flip_flops[dst])
			case ModuleType.CONJUNCTION:
				state.conjunctions[dst][src] = pulse
				out_pulse = Pulse(1 - all(p == Pulse.HIGH for p in state.conjunctions[dst].values()))
			case ModuleType.BROADCAST:
				out_pulse = pulse
			case _:
				assert False, mods[dst].type_

		if out_pulse is not None:
			queue.extend((dst, out_pulse, dd) for dd in mods[dst].dsts)

def n_press_pulses(ip: str, n: int) -> str:
	mods = parse(ip)
	state = State.initial(mods)

	for _ in range(n - 1):
		for _ in pulses(mods, state):
			pass

	p = joinlines(
		f'{src} -{pulse}-> {dst}'
		for src, pulse, dst in pulses(mods, state)
	).strip()

	# print(p)
	return p

def one_press_pulses(ip: str) -> str:
	return n_press_pulses(ip, 1)

def two_press_pulses(ip: str) -> str:
	return n_press_pulses(ip, 2)

def three_press_pulses(ip: str) -> str:
	return n_press_pulses(ip, 3)

def four_press_pulses(ip: str) -> str:
	return n_press_pulses(ip, 4)

def five_press_pulses(ip: str) -> str:
	return n_press_pulses(ip, 5)

def pulse_counts_per_n_presses(ip: str, n: int) -> Counter[Pulse]:
	mods = parse(ip)
	state = State.initial(mods)
	pulse_counts = Counter()

	for _ in range(n):
		for src, pulse, dst in pulses(mods, state):
			pulse_counts[pulse] += 1

	return pulse_counts

def pulse_counts_per_n_presses_csv(ip: str, n: int) -> str:
	counts = pulse_counts_per_n_presses(ip, n)
	return f'{counts[Pulse.LOW]},{counts[Pulse.HIGH]}'

def pulse_counts_per_one_press_csv(ip: str) -> str:
	return pulse_counts_per_n_presses_csv(ip, 1)

def pulse_counts_per_1000_presses_csv(ip: str) -> str:
	return pulse_counts_per_n_presses_csv(ip, 1000)

def p1(ip: str) -> int:
	return prod(pulse_counts_per_n_presses(ip, 1000).values())