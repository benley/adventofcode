#!/usr/bin/env python
"""AOC day 1"""

import sys


def main():
    """Main"""
    data = list(sys.stdin.read())
    pos = 1
    posdone = False
    floor = 0
    for char in data:
        if char == "(":
            floor += 1
        elif char == ")":
            floor -= 1
        if floor < 0 and not posdone:
            print("Went negative at: %d" % pos)
            posdone = True
        pos += 1
    print("Final floor: %d" % floor)


if __name__ == '__main__':
    main()
