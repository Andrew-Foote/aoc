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
	('p1', '95437'),
	('p2', '24933642')
])]

from collections import deque
from dataclasses import dataclass
import functools as ft
from typing import Iterable, Iterator, Optional, Union

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

@dataclass
class File:
	name: str
	parent: 'Dir'
	size: int

@dataclass
class Dir:
	name: str
	parent: Optional['Dir']
	children: list[Union[File, 'Dir']]

	@ft.cached_property
	def size(self) -> int:
		return sum(child.size for child in self.children)

	def __getitem__(self, name: str) -> Union[File, 'Dir']:
		for child in self.children:
			if child.name == name:
				return child

		raise KeyError(name)

	def add_dir(self, name: str) -> 'Dir':
		try:
			return self[name]
		except KeyError:
			new = Dir(name, self, [])
			self.children.append(new)
			return new

	def add_file(self, name: str, size: int) -> File:
		try:
			return self[name]
		except KeyError:
			new = File(name, self, size)
			self.children.append(new)
			return new

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

def iterdirs(root: Dir):
	yield root

	for child in root.children:
		if isinstance(child, Dir):
			yield from iterdirs(child)

def p1(ip: str) -> int:
	cmds = parse(ip)
	root = makefs(cmds)

	return sum(dir_.size for dir_ in iterdirs(root) if dir_.size <= 100_000)

def p2(ip: str) -> int:
	cmds = parse(ip)
	root = makefs(cmds)
	used = root.size
	#used = sum(dir_.size for dir_ in iterdirs(root) if dir_.size <= 100_000)
	print('used', used)
	total_space = 70_000_000
	unused = total_space - used
	print('unused', unused)
	required_unused = 30_000_000
	necessary_to_delete = required_unused - unused
	print('nec to delete', necessary_to_delete)

	return min([
		dir_
		for dir_
		in iterdirs(root)
		if dir_.size >= necessary_to_delete
	], key=lambda dir_: dir_.size).size