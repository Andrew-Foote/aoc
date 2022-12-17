from collections import deque
import heapq
from typing import Iterator, Protocol, Self

_T = TypeVar('_T')

class ChildYielder(Protocol[_T]):
    def __call__(self: Self, node: _T) -> Iterator[_T]:
        """Yield the children of the given node in order."""

def dfs(root: _T, children: ChildYielder[_T]) -> Iterator[_T]:
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

def bfs(root: _T, children: ChildYielder[_T]):
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
                stack.append(iter(children(node)))

def dijkstra(root: _T, children: ChildYielder[_T]):
    queue: list[_T] = [root]
    heapq.heapify(queue)
    visited: set[_T] = set()

    while queue:
        node = heapq.heappop(queue)
        yield node

        for child in children(node):
            if child not in visited:
                visited.add(child)
                heapq.heappush(queue, child)