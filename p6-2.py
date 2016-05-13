#!/usr/bin/env python
"""One-line summary"""

import re


def parse(line):
    match = re.match(
        r'(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)',
        line).groups()
    return (match[0],
            (int(match[1]), int(match[2])),
            (int(match[3]), int(match[4])))


def ceil(a, b):
    return a if a > b else b


def main():
    """Main"""
    row = [0 for _ in range(0, 1000)]
    grid = [row[:] for _ in row]

    with open('p6-input.txt') as fh:
        data = fh.readlines()

    def docmd(cmd, x, y):
        if cmd == 'turn on':
            grid[x][y] += 1
        elif cmd == 'turn off':
            grid[x][y] = ceil(0, grid[x][y] - 1)
        elif cmd == 'toggle':
            grid[x][y] += 2

    for line in data:
        cmd, (xA, yA), (xZ, yZ) = parse(line)
        for x in range(xA, xZ + 1):
            for y in range(yA, yZ + 1):
                docmd(cmd, x, y)

    print sum(sum(r) for r in grid)


if __name__ == '__main__':
    main()
