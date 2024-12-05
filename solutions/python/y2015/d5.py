def is_nice(s: str) -> bool:
    return (
        sum(1 for c in s if c in 'aeiou') >= 3
        and any(c1 == c2 for c1, c2 in zip(s, s[1:]))
        and all(ss not in s for ss in ('ab', 'cd', 'pq', 'xy'))
    )
 
def p1(ip:  str) -> str:
    return sum(1 for s in ip.splitlines() if is_nice(s))

def p2_is_nice_1(s: str) -> bool:
    seen: set[tuple[str, str]] = set()
    prev: tuple[str, str] | None = None

    for c1, c2 in zip(s, s[1:]):
        if (c1, c2) in seen:
            return True

        if prev is not None:
            seen.add(prev)
        
        prev = c1, c2

def p2_is_nice_2(s: str) -> bool:
    return any(c1 == c3 for c1, c2, c3 in zip(s, s[1:], s[2:]))

def p2_is_nice(s: str) -> bool:
    return p2_is_nice_1(s) and p2_is_nice_2(s)

def p2(ip: str) -> str:
    return sum(1 for s in ip.splitlines() if p2_is_nice(s))