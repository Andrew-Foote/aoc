from dataclasses import dataclass
from enum import Enum
import operator
import re
from typing import Callable, Iterator
from utils import joinlines

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