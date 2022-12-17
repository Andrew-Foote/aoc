from collections import deque
from collections.abc import Iterator
import heapq
from typing import Protocol, Self, TypeVar

_T = TypeVar('_T')

class ChildGenerator(Protocol[_T]):
    """A graph encoded as a mapping from each node to an iterator over the node's children."""

    def __call__(self: Self, node: _T) -> Iterator[_T]:
        """Yield the children of the given node in order."""

def dfs(root: _T, children: ChildGenerator[_T]) -> Iterator[_T]:
    """Iterate over the nodes in a tree in depth-first order.
    
    The function can be applied to a graph as well, but it will not filter out nodes that have
    already been visited, so nodes may occur more than once, and if the graph is cyclic, the
    iteration will not terminate."""

    stack: list[Iterator[_T]] = [iter([root])]

    while stack:
        try:
            node: _T = next(stack[-1])
        except StopIteration:
            del stack[-1]
        else:
            yield node
            stack.append(iter(children(node)))

def bfs(root: _T, children: ChildGenerator[_T]) -> Iterator[_T]:
    """Iterate over the nodes in a tree in breadth-first order.
    
    The function can be applied to a graph as well, but it will not filter out nodes that have
    already been visited, so nodes may occur more than once, and if the graph is cyclic, the
    iteration will not terminate."""
    
    queue: deque[Iterator[_T]] = deque([iter([root])])

    while queue:
        try:
            node: _T = next(queue[-1])
        except StopIteration:
            del queue[-1]
        else:
            yield node
            queue.append(iter(children(node)))

# See https://11011110.github.io/blog/2013/12/17/stack-based-graph-traversal.html for information
# on how to correctly implement `gdfs` which I always have to look up.
#  
def gdfs(root: _T, children: ChildGenerator[_T]) -> Iterator[_T]:
    """Iterate over the nodes in a graph in depth-first order, visiting each node exactly once."""
  
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

def gbfs(root: _T, children: ChildGenerator[_T]):
    """Iterate over the nodes in a graph in breadth-first order, visiting each node exactly
    once."""

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

    children = lambda node: iter(example[node])
    assert list(gdfs('s', children)) == ['s', 'a', 'b', 'e', 'd', 'c', 'f', 'g', 'h']
    assert list(gbfs('s', children)) == ['s', 'a', 'c', 'b', 'd', 'f', 'e', 'g', 'h']