from collections.abc import Iterable
from dataclasses import dataclass, field

@dataclass
class UnionFind[T]:
    parent_dict: dict[T, T] = field(default_factory=dict)
    rank_dict: dict[T, int] = field(default_factory=dict)

    def __init__(self, elems: Iterable[T]) -> None:
        self.parent_dict = {}
        self.rank_dict = {}

        for elem in elems:
            self.add_singleton(elem)

    def add_singleton(self, elem: T) -> None:
        self.rank_dict[elem] = 0

    def add_pair(self, a: T, b: T) -> None:
        self.add_singleton(a)
        self.add_singleton(b)
        self.union(a, b)

    def find(self, elem: T) -> T:
        parent_dict = self.parent_dict
        
        try:
            parent = parent_dict[elem]
        except KeyError:
            return elem
        else:
            result = self.find(parent)
            parent_dict[elem] = result
            return result
        
    def union(self, a: T, b: T) -> None:
        a = self.find(a)
        b = self.find(b)

        if a == b:
            return
        
        rank_dict = self.rank_dict
        a_rank = rank_dict[a]
        b_rank = rank_dict[b]

        if a_rank < b_rank:
            a, b = b, a

        self.parent_dict[b] = a

        if a_rank == b_rank:
            rank_dict[a] += 1