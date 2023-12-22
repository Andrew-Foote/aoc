from dataclasses import dataclass
from enum import Enum
import operator
import re
from typing import Callable, Iterator
from utils import joinlines
from solutions.python.lib.utils import prod, range_intersection

test_inputs = [
	('example', '''\
px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}''', [
		('traces', '''\
{x=787,m=2655,a=1222,s=2876}: in -> qqz -> qs -> lnx -> A
{x=1679,m=44,a=2067,s=496}: in -> px -> rfg -> gd -> R
{x=2036,m=264,a=79,s=2244}: in -> qqz -> hdj -> pv -> A
{x=2461,m=1339,a=466,s=291}: in -> px -> qkq -> crn -> R
{x=2127,m=1623,a=2188,s=1013}: in -> px -> rfg -> A'''),
		('rating_sums_csv', '7540,4623,6951'),
		('p1', 19114),
		('p2', 167409079868000)
	])
]

# categories = {'x', 'm', 'a', 's'}
# a part belongs to a category
# a workflow will accept or reject a part
# a workflow has a name and a list of rules
# each rule has a condition and a destination (where a part will
# be sent if the condition is true)
# condition is an operator (<, >, =) plus a number
# destination is either R (reject), A (accept), or a workflow name

class Category(Enum):
	X = 'x'
	M = 'm'
	A = 'a'
	S = 's'

class Operator(Enum):
	LT = '<'
	GT = '>'

	def as_fn(self) -> Callable[[int, int], bool]:
		return {'<': operator.lt, '>': operator.gt}[self.value]

@dataclass
class Rule:
	attr: str
	op: Operator
	val: int
	dst: str

@dataclass
class Workflow:
	rules: list[Rule]
	final_dst: str # the last rule, which has no condition

def parse_workflow(workflow: str) -> tuple[str, Workflow]:
	name, rest = workflow.split('{', 1)
	assert rest[-1] == '}'
	rest = rest[:-1]
	rule_strings = rest.split(',')
	rules = []
	final_dst = rule_strings[-1]
	rule_strings = rule_strings[:-1]

	for rule_s in rule_strings:
		cond, dst = rule_s.split(':')
		m = re.match(r'(\w+)(<|>)(\d+)', cond)
		if m is None: breakpoint()
		attr, op, val = m.groups()
		rules.append(Rule(attr, Operator(op), int(val), dst))

	return name, Workflow(rules, final_dst)

Part = dict[str, int]

def parse_part(part: str) -> Part:
	part = part.strip()
	assert part[0] == '{'
	assert part[-1] == '}'
	part = part[1:-1]
	props = part.split(',')
	part_dict = {}

	for prop in props:
		name, val = prop.split('=')
		part_dict[name] = int(val)

	return part_dict

def part_str(part: Part) -> str:
	bits = ','.join(f'{name}={val}' for name, val in part.items())
	return '{' + bits + '}'

def parse(ip: str) -> tuple[dict[str, Workflow], list[Part]]:
	workflows, parts = ip.split('\n\n')
	workflows = dict(map(parse_workflow, workflows.splitlines()))
	parts = list(map(parse_part, parts.splitlines()))
	return workflows, parts

def apply_workflow(workflow: Workflow, part: Part) -> str:
	for rule in workflow.rules:
		if rule.op.as_fn()(part[rule.attr], rule.val):
			return rule.dst

	return workflow.final_dst

def trace(workflows: dict[str, Workflow], part: Part) -> list[str]:
	workflow_names = ['in']

	while workflow_names[-1] not in ('R', 'A'):
		workflow_names.append(apply_workflow(workflows[workflow_names[-1]], part))

	return workflow_names

def traces(ip: str) -> str:
	workflows, parts = parse(ip)
	lines = []

	for part in parts:
		line = part_str(part) + ': '
		t = trace(workflows, part)
		lines.append(line + ' -> '.join(t))

	return joinlines(lines).strip()

def rating_sums(ip: str) -> Iterator[int]:
	workflows, parts = parse(ip)

	for part in parts:
		accepted = trace(workflows, part)[-1] == 'A'

		if accepted:
			yield sum(part.values())

def rating_sums_csv(ip: str) -> str:
	return ','.join(map(str, rating_sums(ip)))

def p1(ip: str) -> int:
	return sum(rating_sums(ip))

def p2_bruteforce(ip: str) -> int:
	workflows, _ = parse(ip)
	k = 0

	for x in range(1, 4001):
		for m in range(1, 4001):
			for a in range(1, 4001):
				for s in range(1, 4001):
					t = trace(workflows, {'x': x, 'm': m, 'a': a, 's': s})
					print(t)
					input()
					k += t[-1] == 'A'

	return k

#                        a<2006 :          qkq
#       s<1351  : px  -> 
#                        a>=2006,m>2090 :  [A]
#                        a>=2006,m<=2090 : rfg
# in -> 
#       s>=1351 : qqz

# if s < 1351, a >= 2006, m > 2090, then A

# so for a given workflow, we can compute a map of conditions => next workflow
# and we can iteratively expand this map
# a condition will ultimately be a set of ranges
# which the s, a, m, x must fall into

Cond = dict[str, range]

def null_cond() -> Cond:
	return {attr: range(1, 4001) for attr in 'xmas'}

def conjoin_conds(cond1: Cond, cond2: Cond) -> Cond:
	return {attr: range_intersection(cond1[attr], cond2[attr]) for attr in 'xmas'}

def conjoin_cond_with_rule(cond: Cond, rule: Rule) -> Cond:
	cond_range = cond[rule.attr]

	if rule.op == Operator.LT:
		rule_range = range(1, rule.val)
	elif rule.op == Operator.GT:
		rule_range = range(rule.val + 1, 4001)

	new_cond = cond.copy()
	new_cond[rule.attr] = range_intersection(cond_range, rule_range)
	return new_cond

def conjoin_cond_with_negation_of_rule(cond: Cond, rule: Rule) -> Cond:
	cond_range = cond[rule.attr]

	if rule.op == Operator.LT:
		rule_range = range(rule.val, 4001)
	elif rule.op == Operator.GT:
		rule_range = range(1, rule.val + 1)

	new_cond = cond.copy()
	new_cond[rule.attr] = range_intersection(cond_range, rule_range)
	return new_cond

def count_satisfying_cond(cond: Cond) -> int:
	return prod(len(r) for r in cond.values())

def dst_to_cond_map(workflow: Workflow) -> dict[tuple[str, int], Cond]:
	"""Returns a dictionary mapping each destination to the condition required
	for a part to get to that destination when put through the given workflow."""
	else_cond: Cond = null_cond()
	result = {}

	for i, rule in enumerate(workflow.rules):
		cond = conjoin_cond_with_rule(else_cond, rule)
		result[rule.dst, i] = cond
		else_cond = conjoin_cond_with_negation_of_rule(else_cond, rule)

	result[(workflow.final_dst, len(workflow.rules))] = else_cond
	return result

def dstlist_to_cond_map(workflows: dict[str, Workflow]) -> dict[list[tuple[str, int]], Cond]:
	result = {(('in', -1),): null_cond()}

	while True:
		did_something = False

		print('---')
		print(result)
		print()
		print(','.join(f'{dst}:{count_satisfying_cond(cond)}' for dst, cond in result.items()))
		print(sum(count_satisfying_cond(cond) for cond in result.values()))
		print('---')

		for dstlist in list(result.keys()):
			if dstlist[-1][0] not in ('A', 'R'):
				cond_for_dstlist = result.pop(dstlist)
				submap = dst_to_cond_map(workflows[dstlist[-1][0]])
				print('WK: ', dstlist[-1], '####', submap)

				for nextdst, nextdst_cond in submap.items():
					result[dstlist + (nextdst,)] = conjoin_conds(cond_for_dstlist, nextdst_cond)

				did_something = True

		if not did_something:
			return result

def cond_is_empty(cond: Cond) -> bool:
	return any(not r for r in cond.values())

def p2(ip: str) -> int:
	workflows, _ = parse(ip)
	m = dstlist_to_cond_map(workflows)
	k = 0
	k2 = 0
	print(joinlines(' '.join(map(str, dstlist)) + ' : ' + str(cond) for dstlist, cond in m.items()))

	for dstlist1, cond1 in m.items():
		for dstlist2, cond2 in m.items():
			if dstlist1 != dstlist2:
				assert cond_is_empty(conjoin_conds(cond1, cond2))

	for dstlist, cond in m.items():
		if dstlist[-1][0] == 'A':
			k += count_satisfying_cond(cond)
		elif dstlist[-1][0] == 'R':
			k2 += count_satisfying_cond(cond)
		else:
			breakpoint()

	print(k, k2, k + k2, 4000**4)

	return k