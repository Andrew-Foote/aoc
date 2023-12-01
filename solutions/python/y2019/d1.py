# adapted from solution written in 2019

def fuel(mass):
    return mass // 3 - 2

def p1(ip):
    return sum(fuel(int(mass)) for mass in ip.splitlines())

def fuel2(mass):
    # mass // 3 - 2 <= 0 iff mass // 3 <= 2, i.e. mass <= 8
    if mass <= 8:
        return 0

    fuel_ = mass // 3 - 2
    return fuel_ + fuel2(fuel_)

def p2(ip):
    return sum(fuel2(int(mass)) for mass in ip.splitlines())