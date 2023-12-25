from dataclasses import dataclass
from fractions import Fraction as frac
import itertools as it
import math
import numpy as np
from typing import Iterator, Optional, Self
from solutions.python.lib.utils import common_value

test_inputs = [
	('example', '''\
19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3

7,27
''', [
		('p1', 2),
		('rockpos_csv','24,13,10'),
		('p2', 47),
	])
]

@dataclass
class Particle:
	pos: np.ndarray # shape (3,)
	vel: np.ndarray # shape (3,)

	def as2d(self: Self) -> 'Particle':
		return self.__class__(self.pos[:2], self.vel[:2])

def parse(ip: str) -> tuple[int, int, list[Particle]]:
	parts = [p.strip() for p in ip.split('\n\n') if p.strip()]

	if len(parts) == 1:
		parts.append('200000000000000,400000000000000')

	test_min, test_max = map(int, parts[1].split(','))
	lines = parts[0].splitlines()
	hailstones = []

	for line in lines:
		pos_s, vel_s = line.split('@')
		pos = np.array([int(part.strip()) for part in pos_s.split(',')])
		vel = np.array([int(part.strip()) for part in vel_s.split(',')])
		hailstones.append(Particle(pos, vel))

	return test_min, test_max, hailstones

def p1(ip: str) -> int:
	test_min, test_max, hailstones = parse(ip)
	hailstones = [hailstone.as2d() for hailstone in hailstones]
	k = 0

	for h1, h2 in it.combinations(hailstones, 2):
		# path of h1 = { h1.pos + t * h1.vel : t >= 0 }
		# path of h2 = { h2.pos + t * h2.vel : t >= 0 }
		#
		# these are two lines, so they either intersect in exactly one place,
		# they never intersect, or they're identical
		#
		# points of intersection would satisfy the system of equations
		#
		# h1.pos + t1 * h1.vel = h2.pos + t2 * h2.vel,
		# t1 >= 0,
		# t2 >= 0
		# 
		# first here can be rewritten as
		# h1.vel * t1 - h2.vel * t2 = h2.pos - h1.pos
		# i.e.
		# [h1.vel -h2.vel ] (t1) = (h2.pos - h1.pos) 
		# [  \|/     \|/  ] (t2) = (    \|/        )
		#
		# so we just need to solve this linear system, and
		# check that the coordinates in the solution vector are nonnegative
		#
		# if it can't be solved, we can check if the lines are linearly independent
		# by checking if the rank is 2

		A = np.column_stack([h1.vel, -h2.vel])
		b = h2.pos - h1.pos 

		try:
			s = np.linalg.solve(A, b)
		except np.linalg.LinAlgError:
			aug = np.column_stack([A, b])
			consistent = np.linalg.matrix_rank(aug) < np.linalg.matrix_rank(A)

			if not consistent:
				print(h1, h2, 'LINES ARE PARALLEL')
				continue

			# we can probably ignore this case but let's consider it anyway
			# this means the two lines are the same
			# so we can just take the h1 line and ignore the other
			# we need to check if the line contains a point within the test area
			# i.e. there is a t such that t >= 0 and MIN <= h1.pos + t * h1.vel <= MAX
			# i.e. 0 <= t and MIN <= h1.pos[0] + t * h1.vel[0] <= MAX and MIN <= h1.pos[1] + t * h1.vel[1] <= MAX
			# i.e. 0 <= t and (MIN - h1.pos[0])/h1.vel[0] <= t <= (MAX - h1.pos[0])/h1.vel[0]
			#             and (MIN - h1.pos[1])/h1.vel[1] <= t <= (MAX - h1.pos[1])/h1.vel[1]
			#        (swap latter two ineqs if the vels are negative)
			# i.e. max(0, a, b) <= t <= min(A, B)

			lbx, ubx = (test_min - h1.pos[0]) / h1.vel[0], (test_max - h1.pos[0]) / h1.vel[0]
			lby, uby = (test_min - h1.pos[1]) / h1.vel[1], (test_max - h1.pos[1]) / h1.vel[1]

			if h1.vel[0] < 0:
				lbx, ubx = ubx, lbx

			if h1.vel[1] < 0:
				lby, uby = uby, lby

			if max(0, lbx, lby) <= min(ubx, uby):
				k += 1
		else:
			t1, t2 = s

			if t1 < 0 or t2 < 0:
				continue

			p = h1.pos + t1 * h1.vel

			if ((test_min <= p) & (p <= test_max)).all():
				k += 1

	return k

def collision_point(p1: Particle, p2: Particle) -> Optional[np.ndarray]:
	A = np.column_stack([p1.vel, -p2.vel])
	b = p2.pos - p1.pos

	try:
		s = np.linalg.solve(A, b)
	except np.linalg.LinAlgError:
		aug = np.column_stack([A, b])
		consistent = np.linalg.matrix_rank(aug) < np.linalg.matrix_rank(A)

		if consistent:
			breakpoint() # we'll just assume this won't happen
			# or we could use pinv

		if not consistent:
			return None
	else:
		t1, t2 = s

		if t1 < 0 or t2 < 0:
			return None

		return p1.pos + t1 * p2.vel

def does_rock_work(hailstones: list[Particle], rock: Particle) -> bool:
	# does the rock collide with every hailstone?
	return all(collision_point(rock, hailstone) is not None for hailstone in hailstones)

def p2(ip: str) -> int:
	# We seek the unique position p
	# such that there exists a velocity v and times t_1, ..., t_n (one for each hailstone), all non-negative
	# such that p + t_1 v = p_1 + t_1 v_1,
	#           p + t_2 v = p_2 + t_2 v_2,
	#           ...,
	#           p + t_n v = p_n + t_n v_n,
	# where p_i, v_i are the position, velocity of each hailstone
	# 

	# -t_1/t_2 p - t_1 v = -t_1/t_2 p_2 - t_1 v_2
	# p - t_1/t_2 p = p_1 + t_1 v_1 - t_1/t_2 p_2 - t_1 v_2

	# v = p_1/t_1 + v_1 - p/t_1
	#   = p_2/t_2 + v_2 - p/t_2

	# p = p_1 + t_1(v_1 - v) = p_2 + t_2(v_2 - v) = ... = p_n + t_n(v_n - v)
	# so there's n - 1 equations, and n + 1 variables

	# so we need to find a v such that all of these will be equal
	# taking the first pair: we need to find v, t_1, t_2 such that
	# p_1 + t_1(v_1 - v) = p_2 + t_2(v_2 - v)
	# i.e. t_1 v_1 - t_2 v_2 + t_2 v - t_1 v = p_2 - p_1


	# what if v = 1?
	# then it'd be t_1 v_1 - t_2 v_2 + t_2 - t_1 = p_2 - p_1
	# 

	# p + d_1 = p_1 + v_1 t_1 


	# find two hailstones moving in parallel
	# those parallel lines determine a (partially cut) plane
	# the rock line must go along that plane
	# maybe we can pick a third line and just make it so the rock line intersects that line  + the plane

	test_min, test_max, hailstones = parse(ip)

	for h1, h2 in it.combinations(hailstones, 2):			
		A = np.column_stack([h1.vel, -h2.vel])
		b = h2.pos - h1.pos 
		aug = np.column_stack([A, b])
		consistent = np.linalg.matrix_rank(aug) < np.linalg.matrix_rank(A)

		if not consistent:
			break
		
	print(f'hailstones {h1} and {h2} are parallel')

	# so we have two sets like
	# {p1 + t * v : t >= 0}
	# {p2 + t * v : t >= 0}
	# with a common v
	# the plane is determined by p2 - p1, and by v
	# { p1 + m (p2 - p1) + t v : t >= 0 }
	#
	# now we consider a third line { p3 + t * v1 : t >= 0 }
	# the line has to intersect the plane somewhere
	# and the rock's path will have to include that intersection point


	# rock collides with hailstone
	# if there exists t such that rock.pos + t * rock.vel = hailstone.pos + t * hailstone.vel
	# i.e. lines intersect
	# i.e. the matrix equation is consistent
	# i.e. AA^+ b = b
	# where A = np.column_stack([rock.vel, -hailstone.vel])
	# and b = hailstone.pos - rock.pos

	# We're looking for a position (p) and velocity (v)
	# such that a rock thrown along the line { p + tv : t >= 0 }
	# will collide with each hailstone
	# each hailstone h_i's path is a line { P_i + t V_i : t >= 0 }
	# so for each one we need there to exist a t_i >= 0 such that
	#   p + t_i v = P_i + t_i V_i
	#   i.e. t_i (v - V_i) = P_i - p
	#   that is we need P_i - p to be a nonnegative scalar multiple of v - V_i
	# 
	# given two hailstones, the set of all points on lines which intersect both paths
	# will be a plane (assuming the lines are independent)
	# let's say h1 and h2 are the hailstones
	# a line { p + tv : t >= 0 } intersects both paths iff there exist t_1, t_2 such that:
	# p + t_1 v = h1.pos + t_1 * h1.vel
	# p + t_2 v = h2.pos + t_2 * h2.vel
	# t_1 >= 0, t_2 >= 0
	# so we're looking at:
	# { p + tv : t >= 0, exists(t_1, t_2)() }

	# this implies
	# p = h1.pos + t_1 (h1.vel - v) = h2.pos + t_2 (h2.vel - v)
	# so i guess it's { h1.pos + t_1 (h1.vel - v) }

	# i.e. eqsys
	# (vx - h1vx) t_1 = h1px - px
	# (vy - h1vy) t_1 = h1py - py
	# (vz - h1vz) t_1 = h1pz - pz
	# (vx - h2vx) t_2 = h2px - px
	# (vy - h2vy) t_2 = h2py - py
	# (vz - h2vz) t_2 = h2pz - pz

	# the equation is still
	# [h1.vel -h2.vel ] (t1) = (h2.pos - h1.pos) 
	# [  \|/     \|/  ] (t2) = (    \|/        )
	# [               ]      = (               )
	# except we're 3d now, so A is 3 rows, 2 columns
	# well, this gives us the point of intersection of


	#  p = P_0 + t_0 (V_0 - v)
	#    = P_1 + t_1 (V_1 - v)
	#      ...
	# P_0 + t_0 (V_0 - v) = P_1 + t_1 (V_1 - v)
	# V_0 t_0 - V_1 t_1 + t_1 v - t_0 v = P_1 - P_0

	#  t_i = (Px_i - px)/(vx - Vx_i) = (Py_i - py)/(vy - Vy_i) 
	#                        ^ i.e. we need this equation to hold
	# which can also be written as
	# (Px_i - px)(vy - Vy_i) = (Py_i - py)(vx - Vx_i)
	# Px_i vy - Px_i Vy_i - px vy + Vy_i px = Py_i vx - Py_i Vx_i - py vx + Vx_i py
	# Px_i vy + Vy_i px - Py_i vx - Vx_i py = px vy - py vx + Px_i Vy_i - Py_i Vx_i

	# also we only really need to find the value of p, if that helps
	# 

	# rock can be thrown from any integral pos + vel
	# rock needs to collide with every hailstone
	# it will always travel in a straight line, with constant vel

	# so let's say P0 = rock pos (initial), V = rock vel
	# P = P0 + V * T
	# P should be able to intersect each hailstone path at some integer T (diff for each hailstone)
	# so we want to solve for P0, V and each T
	#
	# P0 + V * T1 = H1P0 + H1V * T1
	# P0 + V * T2 = H2P0 + H2V * T2
	# ...
	# 
	# all of the points are on a line
	# so there must be a line going through each hailstone path
	# that's got to imply something about the hailstone paths...
	# 

	#        *
	#     *
	#  *
	#          *
	#
	# since you could have a config like this (where each * is a line going from
	# front to back, along the non-visible axis)
	# you can't draw a single straight line through each of these lines
	# two lines determine a plane
	# a further line need not intersect the plane

	return 1