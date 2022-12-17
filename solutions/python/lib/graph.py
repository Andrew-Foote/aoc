from collections import deque
from collections.abc import Iterator
import heapq
from typing import Protocol, Self, TypeVar

_T = TypeVar('_T')

class ChildGenerator(Protocol[_T]):
    def __call__(self: Self, node: _T) -> Iterator[_T]:
        """Yield the children of the given node in order."""

def dfs(root: _T, children: ChildGenerator[_T]) -> Iterator[_T]:
    # https://11011110.github.io/blog/2013/12/17/stack-based-graph-traversal.html
    # 
    # I always have to look up this blog post since I can never remember where exactly you do the
    # check against the visited set.

    stack: list[Iterator[_T]] = [iter([root])]
    visited: set[_T] = set()

    while stack:
        try:
            node: _T = next(stack[-1])
        except StopIteration:
            del stack[-1]
        else:
            if node not in visited:
                yield node
                visited.add(node)
                stack.append(iter(children(node)))

def bfs(root: _T, children: ChildGenerator[_T]):
    queue: deque[Iterator[_T]] = deque([iter([root])])
    visited: set[_T] = set()

    while queue:
        try:
            node: _T = next(queue[-1])
        except StopIteration:
            del queue[-1]
        else:
            if node not in visited:
                yield node
                visited.add(node)
                queue.appendleft(iter(children(node)))

def dijkstra(root: _T, children: ChildGenerator[_T]):
    queue: list[_T] = [root]
    heapq.heapify(queue)
    visited: set[_T] = {root}

    while queue:
        node: _T = heapq.heappop(queue)
        yield node

        for child in children(node):
            if child not in visited:
                visited.add(child)
                heapq.heappush(queue, child)

def paradfs(root: _T, children: ChildGenerator[_T]):
    stack: list[_T] = [root]
    visited: set[_T] = {root}

    while stack:
        node: _T = stack.pop()
        yield node

        for child in children(node):
            if child not in visited:
                visited.add(child)
                stack.append(child)

if __name__ == '__main__':
    example = {
        's': 'ac',
        'a': 'sbd',
        'b': 'ae',
        'c': 'sdf',
        'd': 'aceg',
        'e': 'bdh',
        'f': 'cg',
        'g': 'dfh',
        'h': 'eg'
    }

    children = lambda node: example[node]
    assert list(dfs('s', children)) == ['s', 'a', 'b', 'e', 'd', 'c', 'f', 'g', 'h']
    assert list(bfs('s', children)) == ['s', 'a', 'c', 'b', 'd', 'f', 'e', 'g', 'h']