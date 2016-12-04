#!/usr/bin/env python
"""look-and-say"""

import itertools
repeat = itertools.repeat


def looksay(num):
    out = ''
    lastc = None
    streak = 1
    for char in str(num):
        if lastc is None:
            lastc = char
            continue
        elif char == lastc:
            streak += 1
            continue
        else:
            out += str(streak)
            out += lastc
            streak = 1
            lastc = char
    out += str(streak)
    out += lastc
    return out


def iterate(func, val):
    return lambda x: reduce(lambda x, f: f(x), repeat(func, val), x)


def main():
    """Main"""
    input = 1113122113
    print len(iterate(looksay, 50)(input))


if __name__ == '__main__':
    main()

