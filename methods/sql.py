import functools as ft
from pathlib import Path
import apsw
import methodlib

# https://rogerbinns.github.io/apsw/tips.html
def _sqlite_error_handler(errcode, message):
    errstr=apsw.mapping_result_codes[errcode & 255]
    print ("SQLITE_LOG: %s (%d) %s %s" % (message, errcode, errstr, apsw.mapping_extended_result_codes.get(errcode, "")))

apsw.config(apsw.SQLITE_CONFIG_LOG, _sqlite_error_handler)

_db = apsw.Connection(':memory:')

_db.execute('''
    create table "input" (
        "id" integer primary key check ("id" = 1), "content" text not null
    ) without rowid;
''')        

@ft.cache
def _load(year: int, day: int) -> None:
    with Path(f'solutions/sql/{year}/{day}.sql').open() as f:
        script = f.read()

    _db.execute(script)

def has_facet(year: int, day: int, facet: str) -> bool:
    _load(year, day)
    
    row = _db.execute(
        'select exists(select * from "sqlite_schema" where "name" = ?)',
        (facet,)
    ).fetchone()

    assert row is not None
    return bool(row[0])

def run_facet(year: int, day: int, facet: str, ip: str) -> str:
    _load(year, day)
    
    _db.execute('''
        insert into "input" ("id", "content") values (1, ?)
        on conflict do update set "content" = "excluded"."content"
    ''', (ip,))

    rows = _db.execute(f'select "answer" from "{facet}"').fetchall()

    if len(rows) != 1:
        raise RuntimeError(
            f"expected a single row in '{facet}' table, got {len(rows)}"
        )

    return str(rows[0][0])

methodlib.register('sql', has_facet, run_facet)