# Velocity is a 2-dimensional vector of integers
# Position is a 2-dimensional vector of integers also
# Position is initially (0, 0)
# On each step, we set
#    pos.x += vel.x
#    pos.y += vel.y
#    vel.x -= sgn(vel.x)    where sgn(x) = { 1 if x > 0, -1 if x < 0, 0 if x = 0 }
# 	 vel.y -= 1
# So this is not a totally linear function because of the sign bit
# However, vel.x will reach 0 in exactly |vel.x| steps, and not change thereafter
# So for those first |vel.x| steps, the formula holds, but thereafter, we can simplify the process to
#
#    pos.y += vel.y
#    vel.y -= 1
#
# We are tasked with finding the initial velocity that
#   a) maximises the maximal y-position
# while
#   b) satisfies the constraint that one of the positions reached, at some point, is within the
#      x- and y- ranges specified in the input.
#
# The y-position will reach its maximum after exactly v0 steps where v0 is the initial y-velocity.
# The y-position at that point will be v0 + (v0 - 1) + ... + 1 = v0 (v0 + 1) / 2
# So to make this as high as possible, we simply need to maximise v0
# 
# Presumably the contraint puts an upper bound on the value of y0, then
# Let's assume the x-bounds will be positive and the y-bounds are negative (this makes sense
# since the target area is a trench ahead of us)
# Intuitively, making v0 too high will cause the probe to overshoot the trench
#
# After reaching the maximum, the y-pos will not change for one step (since the velocity is 0),
# then it will take another v0 steps for the y-pos to change back to 0. y-vel at the start of
# these steps is -1, so at the end of them it will be -(v0 + 1).
#
# So on the next step, the y-pos will decrease to -(v0 + 1). Now if we make that equal to the
# minimum possible value for y, that has to be as big as we can make v0; any bigger, and it would
# overshoot the target area, purely based on the y-changes
#
# This is enough to solve part 1, then --- we don't actually need to know the x-velocity

test_inputs = [('example', 'target area: x=20..30, y=-10..-5', [
	('p1', '45'), ('p2', '112')
])]

import re

def parse(ip: str) -> tuple[int, int, int, int]:
	m = re.match(r'target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)', ip)
	pxmin, pxmax, pymin, pymax = map(int, m.groups())
	return pxmin, pxmax, pymin, pymax

def p1(ip: str) -> int:
	pxmin, pxmax, pymin, pymax = parse(ip)
	vy0 = -(pymin + 1)
	return vy0 * (vy0 + 1) // 2

# OK, so we need to find all initial velocities that put the probe within the target area
# So we know vy(0) can't be greater than -(pymin + 1) (i.e. it must be less than -pymin)
# As for a lower bound, well, if it is less than pymin then it'll immediately go below the target
# area. So assuming the target area doesn't include (0, 0), v0 has to be between pymin and
# -(pymin + 1)
# inclusive

# So we can loop over all those values... then we just need to find the permissible x-values
# Given that vx(0) will be nonnegative, the formula for x over time is:

#   px(t) = vx(0) + (vx(0) - 1) + (vx(0) - 2) + ... + (vx(0) - t)
#         = vx(0)(t + 1) - t(t + 1)/2
#     for t <= vx(0)
#
#   then px(t) remains constant at
#
#   px(vx(0)) = vx(0)(vx(0) + 1) - vx(0)(vx(0) + 1)/2
#             = vx(0)[ vx(0) + 1 - (vx(0) + 1) / 2 ]  
#             = vx(0)(vx(0) + 1)/2
#
# So px(t) has a maximum value of vx(0)(vx(0) + 1)/2
# which means that max value can't be less than pxmin
# what does that mean for vx(0)? 

# vx(0)(vx(0) + 1) / 2 >= pxmin
# vx(0)(vx(0) + 1) >= pxmin / 2
# vx(0)(vx(0) + 1) - pxmin / 2 >= 0
# vx(0)**2 + vx(0) >= pxmin / 2
# vx(0)**2 >= pxmin / 2 - vx(0)
#
# this definitely holds if vx(0) >= pxmin / 2
#
# wait, but we want an upper bound on vx(0)
# well, that's easy --- pxmax
# if it's greater than pxmax, it'll zoom past in 1 go

def p2(ip: str) -> int:
	pxmin, pxmax, pymin, pymax = parse(ip)
	vy0s = range(pymin, -pymin)
	vx0s = range(0, pxmax + 1)
	count = 0

	for vx0 in vx0s:
		for vy0 in vy0s:
			px, py, vx, vy = 0, 0, vx0, vy0

			while True:
				px += vx
				py += vy

				if pxmin <= px <= pxmax and pymin <= py <= pymax:
					count += 1
					break

				if px > pxmax or py < pymin:
					break

				if vx: vx -= (vx // abs(vx))
				vy -= 1

	return count