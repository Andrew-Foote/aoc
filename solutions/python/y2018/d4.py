from dataclasses import dataclass
from enum import Enum

test_inputs = [
	('example_p1', '''
[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up
	''', [
		('p1_guard', '10'),
		('p1_minutes', '50'),
		('p1_minute', '24')
	])
]

@dataclass
class Time:
	year: int
	month: int
	day: int
	hour: int
	minute: int

@dataclass
class Event:
	time: Time

@dataclass
class BeginShiftEvent(Event):
	guard: int

@dataclass
class FallAsleepEvent(Event):
	pass

@dataclass
class WakeUpEvent(Event):
	pass

def parse(ip: str) -> list[Event]:
	for line in ip.splitlines():
		m = re.match(r'\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\] (.*)', line)
		year, month, day, hour, minute, desc = m.groups()
		time = Time(year, month, day, hour, minute)
		m2 = re.match(r'Guard #(\d+) begins shift', desc)

		if m is not None:
			guard = int(m.group(1))
			event = BeginShiftEvent(time, int(guard))
		elif desc == 'falls asleep':
			event = FallAsleepEvent(time)
		elif desc == 'wakes up':
			event = WakeUpEvent(time)
