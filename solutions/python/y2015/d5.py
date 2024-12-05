def is_nice(s: str) -> bool:
    return (
        sum(1 for c in s if c in 'aeiou') >= 3
        and any(c1 == c2 for c1, c2 in zip(s, s[1:]))
        and all(ss not in s for ss in ('ab', 'cd', 'pq', 'xy'))
    )
 
def p1(ip:  str) -> str:
    return sum(1 for s in ip.splitlines() if is_nice(s))