from collections import defaultdict
from collections.abc import Generator, Iterable
from dataclasses import dataclass
import itertools as it
from solutions.python.lib.gint import gint
from solutions.python.lib.grid import Grid, NESW

test_inputs = [('example', '''\
###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############''', [
    ('fastest_time_no_cheating', 84),
    ('cheats_by_amount_saved_csv', '[(2, 14), (4, 14), (6, 2), (8, 4), (10, 2), (12, 3), (20, 1), (36, 1), (38, 1), (40, 1), (64, 1)]'),
    ('num_cheats_saving_at_least_12', 8),
    ('num_cheats_saving_at_least_64', 1),
])]

def parse(ip: str) -> tuple[Grid[str], gint]:
    grid = Grid(ip.splitlines())
    return grid, next(p for p in grid.rect() if grid[p] == 'S')

def is_track(c: str) -> bool:
    return c in '.SE'

def next_track_points(
    grid: Grid[str], cur: gint, prv: gint | None=None
) -> Generator[gint]:
    
    """Yields each adjacent point on the track that the program may visit, given
    that it is currently at `cur` and (if `prv` is not `None`) it has already
    visited `prv`."""

    assert is_track(grid[cur])

    if grid[cur] == 'E':
        return
        
    for d in NESW:
        neighbour = cur + d

        if is_track(grid[neighbour]) and (prv is None or neighbour != prv):
            yield neighbour

    return

def track_points(
    grid: Grid[str], cur: gint, prv: gint | None=None
) -> Generator[gint, None, str]:

    while True:
        yield cur
        nxts = set(next_track_points(grid, cur, prv))

        match len(nxts):
            case 0:
                return cur
            case 1:
                nxt = nxts.pop()
            case _:
                raise RuntimeError('branch found in track!')

        cur, prv = nxt, cur

def track_duration(track: Iterable[gint]) -> int:
    # minus one because we are counting edges, not nodes
    return sum(1 for _ in track) - 1

def fastest_time_no_cheating(ip: str) -> int:
    grid, start = parse(ip)
    return track_duration(track_points(grid, start))

@dataclass(frozen=True)
class Cheat:
    start: gint
    """The point on the no-cheat track the program is at when the cheat is
    activated. The cheat is active during the move from this point to the
    next one, and during the next move after that."""

    end: gint 
    """The point the program is at when the cheat is deactivated. This has
    to be a track point, otherwise a segfault occurs."""

    time: int
    """The time it takes for the program to move to the end point of the
    cheat."""

def available_cheats(grid: Grid[str], start: gint) -> Generator[Cheat]:
    """"Yields a tuple (p, q, t) for each possible cheat, where p is the wall
    the program crossed during the cheat, q is the point adjacent to p that the
    program is at at the end of the cheat, and t is the time it takes to get to
    point q."""

    no_cheat_track = track_points(grid, start)

    for t, p in enumerate(no_cheat_track):
        for d in NESW:
            # The move from p to adj will be the first move the program makes
            # during the cheat.
            adj = p + d

            # This move has to be into a wall. Because the second move has to
            # be into a track point (since the cheat is deactivated while the
            # program is at this point, and a segfault occurs if it's still on
            # a wall). And the cheat is useless if it doesn't go through at
            # least one wall.
            if grid[adj] != '#':
                continue

            for d2 in NESW:
                if d2 == -d:
                    continue

                across = adj + d2

                if across not in grid.rect():
                    continue

                if not is_track(grid[across]):
                    continue

                yield Cheat(p, across, t + 2)

def time_with_cheat(grid: Grid[str], start: gint, cheat: Cheat) -> int | None:
    # at the end of the cheat, there may be multiple paths the program can take
    # e.g. say program is at S here---if it crosses into the middle wall,
    # it has a choice of whether to go up or down afterwards
    #
    # ...
    # .#.
    # S#.
    # .#.
    #
    # however, if it goes up it'll meet the S again
    # it could also theoretically meet a dead end

    if grid[cheat.end] == 'E':
        return cheat.time

    nxts = list(next_track_points(grid, cheat.end))

    if not nxts:
        return None

    times = []

    for nxt in nxts:
        track = list(track_points(grid, nxt, cheat.end))

        if grid[track[-1]] != 'E':
            continue

        times.append(cheat.time + 1 + track_duration(track))

    return min(times)

def cheats_by_amount_saved(ip: str) -> defaultdict[int, set[Cheat]]:
    grid, start = parse(ip)
    time_no_cheat = track_duration(track_points(grid, start))
    result: dict[int, Cheat] = defaultdict(set)

    for cheat in available_cheats(grid, start):
        t = time_with_cheat(grid, start, cheat)

        if t is not None:
            saved = time_no_cheat - t

            if saved > 0:
                result[saved].add(cheat)

    return result

def cheats_by_amount_saved_csv(ip: str) -> str:
    d = cheats_by_amount_saved(ip)
    return str(list(sorted((saved, len(cheats)) for saved, cheats in d.items())))

def cheat_saves_at_least_n(
    grid: Grid[str], start: gint, cheat: Cheat, time_no_cheat: int, n: int
) -> bool:

    if grid[cheat.end] == 'E':
        return time_no_cheat - cheat.time >= n

    # the cheat saves enough time iff
    #   
    #   time_no_cheat - (cheat.time + 1 + track_duration(track)) >= n
    #
    # but given that track_duration(track) >= 0, this will imply
    #
    #   time_no_cheat - (cheat.time + 1) >= n
    #
    # so if this inequality is false, we already know the cheat can't save
    # enough time

    if time_no_cheat - (cheat.time + 1) < n:
        return False

    nxts = list(next_track_points(grid, cheat.end))
    tracks = [track_points(grid, nxt, cheat.end) for nxt in nxts]
    active_track_indices = set(range(len(tracks)))

    for t in range(cheat.time + 1, time_no_cheat - n + 3):
        assert active_track_indices # at least one track must lead to an 'E'

        no_longer_active: set[int] = set()

        for i in active_track_indices:
            track = tracks[i]

            try:
                next(track)
            except StopIteration as e:
                end = e.value

                if grid[end] == 'E':
                    assert time_no_cheat - (t - 2) >= n
                    return True
                    # time_no_cheat >= n + t - 2
                    # t <= time_no_cheat - n + 2
                
                no_longer_active.add(i)
        
        active_track_indices -= no_longer_active

    # print(f'after loop {t=}; {time_no_cheat=}; {n=}')
    # assert time_no_cheat - (t - 2) < n
    return False


    if not nxts:
        return None

    times = []

    for nxt in nxts:
        track = list(track_points(grid, nxt, cheat.end))

        if grid[track[-1]] != 'E':
            continue

        times.append(cheat.time + 1 + track_duration(track))

    return time_no_cheat - min(times) >= n

def num_cheats_saving_at_least_n(ip: str, n: int) -> int:
    grid, start = parse(ip)
    time_no_cheat = track_duration(track_points(grid, start))
    result = 0

    chcount = sum(1 for _ in available_cheats(grid, start))
    print(f'{chcount=}')
    input()

    # probably can filter out a lot of the available_cheats already
    for i, cheat in enumerate(available_cheats(grid, start)):
        if i % 1000 == 0:
            print(f'cheat {i}')

        if cheat_saves_at_least_n(grid, start, cheat, time_no_cheat, n):
            result += 1

    return result

def num_cheats_saving_at_least_12(ip: str) -> int:
    return num_cheats_saving_at_least_n(ip, 12)

def num_cheats_saving_at_least_64(ip: str) -> int:
    return num_cheats_saving_at_least_n(ip, 64)

def p1(ip: str) -> int:
    # program starts at S
    # can move up, down, left or right to track cells
    # each such move takes 1 picosecond
    # program can also disable collision, but only one time
    # during the race
    # the disabling lasts 2 picoseconds and allows the program
    # to pass through walls. program must be on track when the
    # disabling ends
    # we call the tuple of (start position, end position) for
    # the disabling a "cheat"
    # the goal is to count the number of cheats that save at
    # least 100 seconds
    #
    # there are no branches in the input, it's just a linear
    # path

    return num_cheats_saving_at_least_n(ip, 100)
