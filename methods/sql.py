import functools as ft
from pathlib import Path
import sqlite3
import methodlib

_db = sqlite3.connect(':memory:', isolation_level=None)

_db.execute('''
    create table "input" (
        "id" integer primary key check ("id" = 1), "content" text not null
    ) without rowid;
''')        

@ft.cache
def _load(year: int, day: int) -> None:
    with Path(f'solutions/sql/{year}/{day}.sql').open() as f:
        script = f.read()

    _db.executescript(script)

def has_facet(year: int, day: int, facet: str) -> bool:
    _load(year, day)
    
    return bool(_db.execute(
        'select exists(* from "sqlite_schema" where "name" = ?)', (facet,)
    ).fetchone()[0])

def run_facet(year: int, day: int, facet: str, ip: str) -> str:
    _load(year, day)
    _db.execute('insert into "input" ("id", "content") values (1, ?) on conflict replace', (ip,))
    return _db.execute(f'select "answer" from "{facet}"').fetchone()[0]

methodlib.register('sql', has_facet, run_facet)