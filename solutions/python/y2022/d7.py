from abc import ABC, abstractmethod
from dataclasses import dataclass
import functools as ft
from typing import Iterable, Iterator, Optional, Self, Union
from solutions.python.lib import graph

test_inputs = [('example', '''\
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k\
''', [
	('diagram', '''\
- / (dir)
  - a (dir)
    - e (dir)
      - i (file, size=584)
    - f (file, size=29116)
    - g (file, size=2557)
    - h.lst (file, size=62596)
  - b.txt (file, size=14848514)
  - c.dat (file, size=8504156)
  - d (dir)
    - j (file, size=4060174)
    - d.log (file, size=8033020)
    - d.ext (file, size=5626152)
    - k (file, size=7214296)\
'''),
	('p1', '95437'),
	('p2', '24933642')
])]

def parse(ip: str) -> Iterator[tuple]:
	lines = ip.splitlines()

	for line in lines:
		if line.startswith('$ cd '):
			yield ('cd', line[len('$ cd '):])
		elif line == '$ ls':
			yield ('ls',)
		elif line.startswith('dir '):
			yield ('dir', line[len('dir '):])
		else:
			size, fname = line.split()
			yield ('size', int(size), fname)

class FSObj(ABC):
	@property
	@abstractmethod
	def name(self: Self) -> str:
		...

	@property
	@abstractmethod
	def children(self: Self) -> Iterator['FSObj']:
		...

	@property
	@abstractmethod
	def size(self: Self) -> int:
		...

	@abstractmethod
	def diagram(self: Self, indent: int=0) -> str:
		...

@dataclass
class File(FSObj):
	_name: str
	parent: 'Dir'
	_size: int

	@property
	def name(self: Self) -> str:
		return self._name

	@property
	def children(self: Self) -> Iterator[FSObj]:
		yield from ()

	@property
	def size(self: Self) -> int:
		return self._size

	def diagram(self: Self, indent: int=0) -> str:
		return '  ' * indent + f'- {self.name} (file, size={self.size})'

@dataclass
class Dir(FSObj):
	_name: str
	parent: Optional['Dir']
	_children: list[FSObj]

	@property
	def name(self: Self) -> str:
		return self._name

	@property
	def children(self: Self) -> Iterator[FSObj]:
		yield from self._children

	@ft.cached_property
	def size(self: Self) -> int:
		return sum(child.size for child in self.children)

	def __getitem__(self: Self, name: str) -> FSObj:
		for child in self.children:
			if child.name == name:
				return child

		raise KeyError(name)

	def add_dir(self: Self, name: str) -> 'Dir':
		try:
			existing = self[name]
		except KeyError:
			new = Dir(name, self, [])
			self._children.append(new)
			return new
		else:
			assert isinstance(existing, Dir)
			return existing

	def add_file(self: Self, name: str, size: int) -> File:
		try:
			existing = self[name]
		except KeyError:
			new = File(name, self, size)
			self._children.append(new)
			return new
		else:
			assert isinstance(existing, File)
			return existing

	def diagram(self: Self, indent: int=0) -> str:
		name = self.name or '/'
		header = '  ' * indent + f'- {name} (dir)'
		body = [child.diagram(indent + 1) for child in self.children]
		return '\n'.join((header, *body))

def makefs(cmds: Iterable[tuple]) -> Dir:
	root: Dir = Dir('', None, [])
	cd: Dir = root

	for cmd, *args in cmds:
		if cmd == 'cd':
			dirname, = args

			if dirname == '/':
				cd = root
			elif dirname == '..':
				if cd.parent is None:
					raise ValueError('no parent')

				cd = cd.parent
			else:
				cd = cd.add_dir(dirname)
		elif cmd == 'ls':
			pass
		elif cmd == 'dir':
			dirname, = args
			cd.add_dir(dirname)
		elif cmd == 'size':
			size, fname = args
			cd.add_file(fname, size)

	return root

def diagram(ip: str) -> str:
	cmds = parse(ip)
	root = makefs(cmds)
	return root.diagram()

def dirsizes(root: Dir) -> Iterator[int]:
	child_dirs = lambda node: filter(lambda child: isinstance(child, Dir), node.children)

	for d in graph.dfs(root, child_dirs):
		yield d.size

def p1(ip: str) -> int:
	cmds = parse(ip)
	root = makefs(cmds)
	return sum(filter(lambda dirsize: dirsize <= 100_000, dirsizes(root)))

def p2(ip: str) -> int:
	cmds = parse(ip)
	root = makefs(cmds)
	used = root.size
	total_space = 70_000_000
	unused = total_space - used
	required_unused = 30_000_000
	necessary_to_delete = required_unused - unused
	return min(filter(lambda dirsize: dirsize >= necessary_to_delete, dirsizes(root)))