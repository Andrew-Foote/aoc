from pathlib import Path
import datetime
import functools as ft
import os
import re
import sqlite3
import tomllib
from typing import Iterator
import urllib.parse
import urllib.request

SETTINGS_PATH = Path('settings.toml')
INPUT_PATH = Path('input')

class ConfigError(Exception):
    pass

class AOC:
    firefox_profile_path: Path
    firefox_container_origin_attributes: str

    def __init__(self):
        if not SETTINGS_PATH.exists():
            raise ConfigError(
                'No settings file found. Read settings.toml.example, make the appropriate '
                f'adjustments to the settings and save a copy at {SETTINGS_PATH}.'
            )

        with SETTINGS_PATH.open('rb') as f:
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

        if not firefox_cookie_db_path.exists():
            raise ConfigError(
                f'Unable to find the Firefox cookie database at {firefox_cookie_db_path}.'
            )

        db = sqlite3.connect(firefox_cookie_db_path)

        session_cookie_row = db.execute(
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
    def common_request_headers(self):
        return {
            'Cookie': f'session={self.session_cookie}',
            'User-Agent': self.user_agent
        }

    @ft.cache
    def input(self, day: int, year: int) -> str:
        dir_path = INPUT_PATH / str(year)
        dir_path.mkdir(parents=True, exist_ok=True)
        path = dir_path / f'{day}.txt'

        if path.exists():
            with path.open(encoding='utf-8') as f:
                return f.read()

        url = f'https://adventofcode.com/{year}/day/{day}/input'
        request = urllib.request.Request(url, headers=self.common_request_headers)
        print(f'[request log] GET {url}')
        response = urllib.request.urlopen(request)
        
        with response:
            content = response.read().decode('utf-8')

        with path.open('w', encoding='utf-8') as f:
            f.write(content)

        return content

    def submit_answer(self, answer: str, part: int, day: int, year: int) -> str:
        lock_path = Path('lock.txt')

        if lock_path.exists():
            with lock_path.open('r', encoding='utf-8') as f:
                lock_created = datetime.datetime.fromisoformat(f.read().strip())

                while (datetime.datetime.now() - lock_created).total_seconds() <= 60:
                    input('The website has asked us to wait one minute before trying again. ')

        url = f'https://adventofcode.com/{year}/day/{day}/answer'
        data = {'level': str(part), 'answer': answer}
        data = urllib.parse.urlencode(data).encode('us-ascii')
        request = urllib.request.Request(url, data, headers=self.common_request_headers)
        print(f'[request log] POST {url}')
        response = urllib.request.urlopen(request)

        with response:
            content = response.read().decode('utf-8')

        body = re.search(r'<main>(.*)</main>', content, re.DOTALL).group(1)

        # strip html tags
        body = re.sub(r'<.*?>', '', body, re.DOTALL)

        if 'Please wait one minute before trying again' in body:
            with lock_path.open('w', encoding='utf-8') as f:
                f.write(str(datetime.datetime.now()))

        return body
        # <span class="day-success">
        
if __name__ == '__main__':
    import sys

    if len(sys.argv) > 4:
        raise RuntimeError('Too many arguments, expected at most 3 (part, day, year).')

    part = int(sys.argv[1]) if len(sys.argv) > 1 else 1
    day = int(sys.argv[2]) if len(sys.argv) > 2 else datetime.date.today().day
    year = int(sys.argv[3]) if len(sys.argv) > 3 else datetime.date.today().year

    import solutions
    year_module = getattr(solutions, f'y{year}')
    day_module = getattr(year_module, f'd{day}')

    test_part_method = getattr(day_module, f'test_p{part}')    
    test_part_method()

    part_method = getattr(day_module, f'p{part}')

    aoc = AOC()
    input_ = aoc.input(day, year)
    answer = part_method(input_)
    print(answer)

    should_submit = input('Submit answer? (y/n) ') == 'y'

    if should_submit:
        response = aoc.submit_answer(answer, part, day, year)
        print(f'Response:\n{response}')
