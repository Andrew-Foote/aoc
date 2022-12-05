from typing import Iterable

def joinlines(lines: Iterable[str]) -> str:
	return ''.join(line + '\n' for line in lines)

def newlineescape(s: str) -> str:
	return s.translate({ord('\n'): '\\n'})