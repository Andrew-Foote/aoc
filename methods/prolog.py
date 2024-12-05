import subprocess
import methodlib
from pathlib import Path

def escape(ip: str) -> str:
    return ip

def has_facet(year: int, day: int, facet: str):
    if facet == 'p1':
        return True

def run_facet(year: int, day: int, facet: str, ip: str) -> str:
    path = Path(f'solutions/prolog/{year}/{day}.pro')    
    
    return subprocess.check_output([
        'swipl', '-s', str(path), '-g', f'{facet}(`{escape(ip)}`)', '-t', 'halt'
    ], encoding='utf-8')

methodlib.register('prolog', has_facet, run_facet)