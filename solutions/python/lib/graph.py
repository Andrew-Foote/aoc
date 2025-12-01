from collections import deque
from collections.abc import Generator, Iterable, Iterator
from dataclasses import dataclass, field
import heapq
from typing import Protocol, Self

class ChildGenerator[T](Protocol):
    """A graph encoded as a mapping from each node to an iterable which yields
    the node's children."""

    def __call__(self: Self, node: T) -> Iterable[T]:
        """Yield the children of the given node in order."""

def dfs[T](root: T, children: ChildGenerator[T]) -> Generator[T]:
    """Iterate over the nodes in a tree in depth-first order.
    
    The function can be applied to a graph as well, but it will not filter out
    nodes that have already been visited, so nodes may occur more than once, and
    if the graph is cyclic, the iteration will not terminate."""

    stack: list[Iterator[T]] = [iter([root])]

    while stack:
        try:
            node = next(stack[-1])
        except StopIteration:
            del stack[-1]
        else:
            yield node
            stack.append(iter(children(node)))

def bfs[T](root: T, children: ChildGenerator[T]) -> Generator[T]:
    """Iterate over the nodes in a tree in breadth-first order.
    
    The function can be applied to a graph as well, but it will not filter out
    nodes that have already been visited, so nodes may occur more than once, and
    if the graph is cyclic, the iteration will not terminate."""
    
    queue: deque[Iterator[T]] = deque([iter([root])])

    while queue:
        try:
            node = next(queue[-1])
        except StopIteration:
            del queue[-1]
        else:
            yield node
            queue.appendleft(iter(children(node)))

class gdfs[T]:
    """A generator which visits each node in a graph exactly once, in depth-
    first order.

    The visited set is accessible as a public property of the generator."""

    stack: list[Iterator[T]]
    visited: set[T]

    def __init__(self: Self, root: T, children: ChildGenerator[T]) -> None:
        self.stack = [iter([root])]
        self.visited = set()
        self.children = children

    def __next__(self: Self) -> T:
        while True:
            try:
                children_iterator = self.stack[-1]
            except IndexError:
                raise StopIteration

            try:
                node = next(children_iterator)
            except StopIteration:
                del self.stack[-1]
            else:
                if node not in self.visited:
                    break

        self.visited.add(node)
        self.stack.append(iter(self.children(node)))
        return node

    def __iter__(self: Self) -> Self:
        return self

class gbfs[T]:
    """A generator which visits each node in a graph exactly once, in breadth-
    first order.

    The visited set is accessible as a public property of the generator."""
    queue: deque[Iterator[T]]
    visited: set[T]

    def __init__(self: Self, root: T, children: ChildGenerator[T]) -> None:
        self.queue = deque([iter([root])])
        self.visited = set()
        self.children = children

    def __next__(self: Self) -> T:
        while True:
            try:
                children_iterator = self.queue[-1]
            except IndexError:
                raise StopIteration

            try:
                node = next(children_iterator)
            except StopIteration:
                del self.queue[-1]
            else:
                if node not in self.visited:
                    break

        self.visited.add(node)
        self.queue.appendleft(iter(self.children(node)))
        return node

    def __iter__(self: Self) -> Self:
        return self

class WeightedChildGenerator[T](Protocol):
    """A weighted graph encoding as a mapping from each node to an iterable
    which yields tuples (c, k), where c is a child and k is the weight of the
    edge from the node to c, one for each child."""
 
    def __call__(self: Self, node: T) -> Iterable[tuple[T, int]]:
        ...

@dataclass(frozen=True, order=True)
class DijkstraNode[T]:
    node: T = field(compare=False)
    cost: int
    parent: 'DijkstraNode | None' = None

class dijkstra[T]:
    heap: list[DijkstraNode[T]]
    visited: set[T]

    def __init__(
        self: Self, root: T, children: WeightedChildGenerator[T]
    ) -> None:
        
        self.heap = [DijkstraNode(root, 0)]
        heapq.heapify(self.heap)
        self.visited = set()
        self.children = children
        # we need the set of all nodes that are visited on a best path
        # so for each terminus, it'd be nice to hjave the path that led up
        # to that terminus

    def __next__(self: Self) -> DijkstraNode[T]:
        if not self.heap:
            raise StopIteration
        
        dnode = heapq.heappop(self.heap)
        node = dnode.node
        cost = dnode.cost

        for child, child_cost in self.children(node):
            if child not in self.visited:
                heapq.heappush(
                    self.heap,
                    DijkstraNode(child, cost + child_cost, dnode)
                )

                self.visited.add(child)

        return dnode

    def __iter__(self: Self) -> Self:
        return self
    
    def path(self: Self, node: DijkstraNode[T]) -> Generator[DijkstraNode[T]]:
        cur = node
        yield cur

        while cur.parent is not None:
            cur = cur.parent
            yield cur

# def dijkstra(root: _T, children: ChildGenerator[_T]):
#     queue: list[_T] = [root]
#     heapq.heapify(queue)
#     visited: set[_T] = {root}

#     while queue:
#         node: _T = heapq.heappop(queue)
#         yield node

#         for child in children(node):
#             if child not in visited:
#                 visited.add(child)
#                 heapq.heappush(queue, child)

# Incorrect implementation of dfs with a visited set, left as a warning! See
# https://11011110.github.io/blog/2013/12/17/stack-based-graph-traversal.html.
def dfs_bad[T](root: T, children: ChildGenerator[T]) -> Generator[T]:
    stack: list[T] = [root]
    visited: set[T] = {root}

    while stack:
        node = stack.pop()
        yield node

        for child in children(node):
            if child not in visited:
                visited.add(child)
                stack.append(child)
