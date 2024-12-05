from abc import ABC
import datetime
import functools as ft
import importlib
import logging
import os
from pathlib import Path
import re
import sqlite3
import sys
import tempfile
import tomllib
from typing import Iterator, Optional
import urllib.parse
import urllib.request
import methodlib
from methods import haskell, python, sql
from utils import newlineescape

logging.basicConfig(
    filename='requests.log', encoding='utf-8', level=logging.INFO,
    format='%(asctime)s %(levelname)-8s %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)

class ConfigError(Exception):
    pass

class AOC:
    firefox_profile_path: Path
    firefox_container_origin_attributes: str

    def __init__(self):
        settings_path = Path('settings.toml')

        if not settings_path.exists():
            raise ConfigError(
                'No settings file found. Read settings.toml.example, make the appropriate '
                f'adjustments to the settings and save a copy at {SETTINGS_PATH}.'
            )

        with settings_path.open('rb') as f:
            _dict = tomllib.load(f)

        self.firefox_profile_path = Path(_dict['firefox_profile_path'])

        if not self.firefox_profile_path.exists():
            raise ConfigError(
                f'Unable to find a Firefox profile at {self.firefox_profile_path}. Most likely the'
                ' firefox_profile_path in settings.toml is incorrect.'
            )

        self.firefox_container_origin_attributes = _dict.get('firefox_container_origin_attributes', '')

        self.user_agent = _dict.get('user_agent')

    @ft.cached_property
    def session_cookie(self) -> str:
        firefox_cookie_db_path = self.firefox_profile_path / 'cookies.sqlite'
        print(firefox_cookie_db_path)

        if not firefox_cookie_db_path.exists():
            raise ConfigError(
                f'Unable to find the Firefox cookie database at {firefox_cookie_db_path}.'
            )

        # Since Firefox may lock the cookie database, we copy it to a temporary
        # file before opening.

        with tempfile.TemporaryDirectory() as tempdir:
            db_copy_path = Path(tempdir) / 'cookies.sqlite'

            with firefox_cookie_db_path.open('rb') as db_file:
                with db_copy_path.open('wb') as db_copy_file:
                    db_copy_file.write(db_file.read())

            firefox_cookie_db = sqlite3.connect(db_copy_path)

            session_cookie_row = firefox_cookie_db.execute(
                '''
                    select "value" from "moz_cookies" as "current"
                    where "name" = ? and "host" = ? and "path" = ? and "originAttributes" = ?
                ''',
                ('session', '.adventofcode.com', '/', self.firefox_container_origin_attributes)
            ).fetchone()

            if session_cookie_row is None:
                raise ConfigError(
                    f'The Firefox profile directory "{firefox_cookie_db_path}" does not contain an Advent of '
                    'Code session cookie. You will need to log into the website to create a new one.'
                )

            return session_cookie_row[0]

    @ft.cached_property
    def db(self):
        db_path = Path('db.sqlite')
        exists = db_path.exists()
        conn = sqlite3.connect(db_path)

        if not exists:
            conn.executescript('''
                -- The input for the AoC {year} puzzle for day {day} was {content}.
                create table "input" (
                    "year" integer check ("year" >= 120),
                    "day" integer check ("day" >= 1 and "day" <= 25),
                    "content" text not null,
                    primary key ("year", "day")
                ) without rowid;

                -- {number} is a possible part number for an AoC puzzle.
                create table "part" ("number" integer primary key);
                insert into "part" ("number") values (1), (2);

                -- The answer for part {part} of the AoC {year} puzzle for day {day} was {content}.
                create table "answer" (
                    "year" integer,
                    "day" integer,
                    "part" integer,
                    "content" text not null,
                    primary key ("year", "day", "part"),
                    foreign key ("year", "day") references "input" ("year", "day"),
                    foreign key ("part") references "part" ("number")
                ) without rowid;

                -- At the time corresponding to the Unix timestamp {timestamp}, the user used this
                -- script to submit an answer for part {part} of the AoC {year} puzzle for day
                -- {day} to the AoC site, and the AoC site responded by saying that the answer was
                -- incorrect.
                create table "failed_submission" (
                    "timestamp" integer,
                    "year" integer not null,
                    "day" integer not null,
                    "part" integer not null,
                    primary key ("timestamp"),
                    foreign key ("year", "day") references "input" ("year", "day"),
                    foreign key ("part") references "part" ("number")
                ) without rowid;

                -- {content} is an example input for the AoC {year} puzzle for day {year}, which
                -- can be used for testing. It can be referred to by the name {name}.
                create table "test_input" (
                    "year" integer,
                    "day" integer,
                    "name" text,
                    "index" integer not null,
                    "content" text not null,
                    primary key ("year", "day", "name"),
                    unique ("year", "day", "index"),
                    unique ("year", "day", "content"),
                    foreign key ("year", "day") references "input" ("year", "day")
                ) without rowid;

                create table "test_facet" (
                    "year" integer,
                    "day" integer,
                    "name" text,
                    "index" integer not null,
                    primary key ("year", "day", "name"),
                    foreign key ("year", "day") references "input" ("year", "day")                    
                ) without rowid;

                -- {content} is the answer to a question that can be asked about the example input
                -- identified by {year}, {day}, {input}. The question is identified by the string
                -- {facet}.
                create table "test_answer" (
                    "year" integer,
                    "day" integer,
                    "input" text,
                    "facet" text,
                    "content" text not null,
                    primary key ("year", "day", "input", "facet"),
                    foreign key ("year", "day", "input")
                        references "test_input" ("year", "day", "name") on delete cascade,
                    foreign key ("year", "day", "facet")
                        references "test_facet" ("year", "day", "name") on delete cascade
                ) without rowid;

                -- The user has used this script to test an answer to part {part} of the AoC {year}
                -- puzzle for day {day} using method {method}, and the answer was found to be
                -- correct.
                create table "method_completed" (
                    "method" text,
                    "year" integer,
                    "day" integer,
                    "part" integer,
                    primary key ("method", "year", "day", "part"),
                    foreign key ("year", "day") references "input" ("year", "day"),
                    foreign key ("part") references "part" ("number")
                ) without rowid;
            ''')

        return conn

    @ft.cached_property
    def common_request_headers(self):
        return {
            'Cookie': f'session={self.session_cookie}',
            'User-Agent': self.user_agent
        }

    def sync_input(self, year: int, day: int) -> str:
        dir_path = Path('input') / str(year)
        dir_path.mkdir(parents=True, exist_ok=True)
        input_path = dir_path / f'{day}.txt'

        if input_path.exists():
            with input_path.open('r', encoding='utf-8') as f:
                content = f.read()

                with self.db:
                    self.db.execute('''
                        insert into "input" ("year", "day", "content")
                        values (?, ?, ?) on conflict do update
                        set "content" = "excluded"."content"
                    ''', (year, day, content))

    @ft.cache
    def input(self, year: int, day: int) -> str:
        # try to get it from the DB first
        row = self.db.execute(
            'select "content" from "input" where "year" = ? and "day" = ?',
            (year, day)
        ).fetchone()

        if row is not None:
            return row[0]

        # if not, try the filesystem --- maybe we manually downloaded it
        dir_path = Path('input') / str(year)
        dir_path.mkdir(parents=True, exist_ok=True)
        input_path = dir_path / f'{day}.txt'

        if input_path.exists():
            with input_path.open('r', encoding='utf-8', newline='\n') as f:
                content = f.read()

                with self.db:
                    self.db.execute('''
                        insert into "input" ("year", "day", "content")
                        values (?, ?, ?)
                    ''', (year, day, content))

            return content

        # last resort: send a HTTP request to the website
        url = f'https://adventofcode.com/{year}/day/{day}/input'
        request = urllib.request.Request(url, headers=self.common_request_headers)
        response = urllib.request.urlopen(request)
        logging.info('GET %s', url)
        
        with response:
            content = response.read().decode('utf-8')

        with self.db:
            self.db.execute(
                'insert into "input" ("year", "day", "content") values (?, ?, ?)',
                (year, day, content)
            )

        # copy inputs to a file as well, for greater visibility --- DB is the source of truth
        # though
        dir_path = Path('input') / str(year)
        dir_path.mkdir(parents=True, exist_ok=True)

        with (dir_path / f'{day}.txt').open('w', encoding='utf-8', newline='\n') as f:
            f.write(content)

        return content

    def register_answer(self, year: int, day: int, part: int, answer: str) -> None:
        with self.db:
            self.db.execute('''
                insert into "answer" ("year", "day", "part", "content")
                values (?, ?, ?, ?)
                on conflict do nothing
            ''', (year, day, part, answer))

    def submit_answer(self, year: int, day: int, part: int, answer: str) -> str:
        while self.db.execute('''select exists(
            select * from "failed_submission" where "year" = ? and "day" = ? and "part" = ?
            and ? - "timestamp" <= 60
        )''', (year, day, part, datetime.datetime.now().timestamp())).fetchone()[0]:
            input(
                'You already submitted an answer for this problem within the past minute. You need'
                'to wait a minute before trying again.'
            )

        url = f'https://adventofcode.com/{year}/day/{day}/answer'
        data = {'level': str(part), 'answer': answer}
        encoded_data = urllib.parse.urlencode(data)
        
        request = urllib.request.Request(
            url,
            encoded_data.encode('us-ascii'),
            headers=self.common_request_headers
        )

        response = urllib.request.urlopen(request)
        logging.info(f'POST {url} {encoded_data}')

        with response:
            content = response.read().decode('utf-8')

        body_match = re.search(r'<main>(.*)</main>', content, re.DOTALL)

        if body_match is None:
            print('Unexpected response format---no <main> tag! Assuming it was the wrong answer...\n')
            return False, content

        body = body_match.group(1)

        if '<span class="day-success">' in body:
            success = True
            self.register_answer(year, day, part, answer)
        elif "That's not the right answer" in body:
            success = False

            with self.db:
                self.db.execute('''
                    insert into "failed_submission" ("timestamp", "year", "day", "part")
                    values (?, ?, ?, ?)
                ''', (datetime.datetime.now().timestamp(), year, day, part))
        elif "You don't seem to be solving the right level" in body:
            print(
                'You have already submitted an answer for this puzzle but the answer was not '
                'recorded by this tool.'
            )

            success = self.find_submitted_answers_and_compare(year, day, part, answer)
        else:
            print('Unexpected response format! Assuming it was the wrong answer...\n')
            success = False

        # strip html tags for display
        body = re.sub(r'<.*?>', '', body, flags=re.DOTALL)

        return success, body

    def find_submitted_answers(self, year: int, day: int) -> bool:
        url = f'https://adventofcode.com/{year}/day/{day}'        
        request = urllib.request.Request(url, headers=self.common_request_headers)
        response = urllib.request.urlopen(request)
        logging.info(f'GET {url}')

        with response:
            content = response.read().decode('utf-8')

        answers = [
            m.group(1) for m in
            re.finditer(r'Your puzzle answer was <code>(.*?)</code>', content)
        ]

        if len(answers) not in (1, 2):
            print(
                'Unexpected response format when trying to find the values of already-submitted '
                f'answers on the AoC site---{len(answers)} were found using the regex '
                '/Your puzzle answer was <code>(.*?)</code>/. Here is the response in full:\n'
            )

            print(content)
        
        return {i + 1: answer for i, answer in enumerate(answers)}

    def find_submitted_answers_and_compare(self, year: int, day: int, part: int, answer: str):
        submitted_answers = self.find_submitted_answers(year, day)

        try:
            submitted_answer = submitted_answers[part]
        except KeyError:
            print(f'Could not find the previously-submitted answer for part {part}.')
            success = False
        else:
            self.register_answer(year, day, part, submitted_answer)

            success = submitted_answer == answer
            like_or_unlike = 'like' if success else 'unlike'

            print(
                f'The previously-submitted answer was {submitted_answer}, {like_or_unlike} the'
                ' one you were trying to submit.'
            )

        return success

    def method_completed(self, method: str, year: int, day: int, part: int) -> bool:
        return self.db.execute('''
            select exists(
                select * from "method_completed"
                where "method" = ? and "year" = ? and "day" = ? and "part" = ?
            )
        ''', (method, year, day, part)).fetchone()[0]

    def tests(self, year: int, day: int) -> list[tuple[int, str, str, str]]:
        return list(self.db.execute('''
            select
                "test_input"."name", "test_input"."content",
                "test_answer"."facet", "test_answer"."content"
            from "test_input"
            join "test_answer" on
                "test_input"."year" = "test_answer"."year"
                and "test_input"."day" = "test_answer"."day"
                and "test_input"."name" = "test_answer"."input"
            join "test_facet" on
                "test_answer"."year" = "test_facet"."year"
                and "test_answer"."day" = "test_facet"."day"
                and "test_answer"."facet" = "test_facet"."name"
            where "test_input"."year" = ? and "test_input"."day" = ?
            order by "test_input"."index", "test_facet"."index"
        ''', (year, day)))

    def maybe_answer(self, year: int, day: int, part: int) -> Optional[str]:
        row = self.db.execute(
            'select "content" from "answer" where "year" = ? and "day" = ? and "part" = ?',
            (year, day, part)
        ).fetchone()

        if row is None:
            return None

        return row[0]

    def interpret_test_defs(self, year: int, day: int, test_defs: methodlib.TestDefs) -> None:
        with self.db:
            self.db.execute('delete from "test_answer" where "year" = ? and "day" = ?', (year, day))
            self.db.execute('delete from "test_facet" where "year" = ? and "day" = ?', (year, day))
            self.db.execute('delete from "test_input" where "year" = ? and "day" = ?', (year, day))

            for i, (name, input_, facets) in enumerate(test_defs):
                self.db.execute('''
                    insert into "test_input" ("year", "day", "name", "index", "content")
                    values (?, ?, ?, ?, ?)
                    on conflict do update set
                        "index" = "excluded"."index", "content" = "excluded"."content"
                ''', (year, day, name, i, input_))

                for i, (facet, answer) in enumerate(facets):
                    self.db.execute('''
                        insert into "test_facet" ("year", "day", "name", "index")
                        values (?, ?, ?, ?)
                        on conflict do update set "index" = "excluded"."index"
                    ''', (year, day, facet, i))

                    self.db.execute('''
                        insert into "test_answer" ("year", "day", "input", "facet", "content")
                        values (?, ?, ?, ?, ?)
                        on conflict do update set "content" = "excluded"."content"
                    ''', (year, day, name, facet, answer))

    def register_completed_method(self, method_name: str, year: int, day: int, part: int) -> None:
        with self.db:
            self.db.execute('''
                insert into "method_completed" ("method", "year", "day", "part")
                values (?, ?, ?, ?) on conflict do nothing
            ''', (method_name, year, day, part))

if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser(
        prog = 'Advent of Code Tool',
        description = 'Automates aspects of solving Advent of Code problems.'
    )

    today = datetime.date.today()
    parser.add_argument('-m', '--method', choices=methodlib.methods.keys(), default='python')
    parser.add_argument('-y', '--year', type=int)
    parser.add_argument('-d', '--day', type=int)
    parser.add_argument('-p', '--part', type=int)
    parser.add_argument('-s', '--sync-input', action='store_true')
    args = parser.parse_args()

    method_name = args.method

    try:
        method = methodlib.methods[method_name]
    except KeyError:
        print(f"'{method_name}' is not a recognized method name.")
        sys.exit()

    if args.year is None:
        if today.month == 12:
            year = today.year
        else:
            year = today.year - 1
    else:
        year = args.year

    if args.day is None:
        if year == today.year and today.month == 12 and today.day <= 25:
                day = today.day
        else:
            print(
                f'There is no puzzle today for Advent of Code {year}. Please specify the day using'
                ' the -d option.'
            )
            sys.exit()
    else:
        day = args.day

    aoc = AOC()

    if args.sync_input:
        print(f'Syncing input for year {year}, day {day} from filesystem.')
        aoc.sync_input(year, day)

    if args.part is None:
        if aoc.method_completed(method_name, year, day, 1):
            part = 2
        else:
            part = 1
    else:
        part = args.part

    print(f"Running '{method_name}' solution for Advent of Code {year}, day {day}, part {part}.")

    aoc.interpret_test_defs(year, day, method.test_defs(year, day))

    print('\nTest results:\n')
       
    has_at_least_one_test = False

    for test_input_name, test_input, facet, expected_test_answer in aoc.tests(year, day):
        has_at_least_one_test = True
        print(f"Input '{test_input_name}', facet '{facet}': ", end='')

        if method.has_facet(year, day, facet):
            answer = method.run_facet(year, day, facet, test_input)

            if answer != expected_test_answer:
                print(f"failed (got '{newlineescape(answer)}', expected '{newlineescape(expected_test_answer)}')")
            else:
                print(f"succeeded (got '{newlineescape(answer)}' as expected)")
        else:
            print('no test available')

    if has_at_least_one_test:
        input('\nAre you happy with the test results? Press Enter to continue, Ctrl+C to cancel.')
    else:
        input('There are no tests to run! Maybe you should add some?')

    answer = method.run_part(year, day, part, aoc.input(year, day))
    print(f'Answer:\n{answer}')

    if (expected_answer := aoc.maybe_answer(year, day, part)) is None:
        opt = input(
            'An answer for this puzzle has not been recorded already. Options:\n'
            '  (n) do nothing\n'
            '  (e) try to get the answer from the Advent of Code website '
              "(if you've already submitted an answer to this puzzle manually)\n"
            '  (s) submit the answer to the Advent of Code website\n'
        )

        if opt == 's':
            success, response = aoc.submit_answer(year, day, part, answer)
            print(f'Response:\n{response}')

            if success:
                aoc.register_completed_method(method_name, year, day, part)
        elif opt == 'e':
            success = aoc.find_submitted_answers_and_compare(year, day, part, answer)

            if success:
                aoc.register_completed_method(method_name, year, day, part)
    elif expected_answer == answer:
        print('Correct.')
        aoc.register_completed_method(method_name, year, day, part)
    else:
        print(f'Incorrect. The correct answer was {expected_answer}.')
