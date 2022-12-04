#!/usr/bin/env python
"""One-line summary"""

import collections


def part1(data):
    x, y = 0, 0
    houses = collections.defaultdict(int, {(0, 0): 1})
    for char in data:
        if char == "^":
            y += 1
        elif char == "v":
            y -= 1
        elif char == "<":
            x -= 1
        elif char == ">":
            x += 1
        else:
            raise RuntimeError("what the fuck? %s" % char)
        houses[(x, y)] += 1
    gotpresents = [k for k, v in houses.items() if v > 0]

    print("Part 1: %s" % len(gotpresents))


def part2(data):
    xS, yS = 0, 0
    xR, yR = 0, 0
    houses = collections.defaultdict(int, {(0, 0): 2})
    idata = iter(data)
    try:
        while True:
            char = idata.next()
            if char == "^":
                yS += 1
            elif char == "v":
                yS -= 1
            elif char == "<":
                xS -= 1
            elif char == ">":
                xS += 1
            else:
                raise RuntimeError
            char = idata.next()
            if char == "^":
                yR += 1
            elif char == "v":
                yR -= 1
            elif char == "<":
                xR -= 1
            elif char == ">":
                xR += 1
            else:
                raise RuntimeError
            houses[(xS, yS)] += 1
            houses[(xR, yR)] += 1
    except StopIteration:
        gotpresents = [k for k, v in houses.items() if v > 0]

    print("Part 2: %s" % len(gotpresents))



def main():
    """Main"""
    data = open("D3/input.txt").read().strip()
    part1(data)
    part2(data)


if __name__ == '__main__':
    main()
