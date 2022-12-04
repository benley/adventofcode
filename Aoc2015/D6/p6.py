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


def main():
    """Main"""
    row = [False for _ in range(0, 1000)]
    grid = [row[:] for _ in row]

    with open('p6-input.txt') as fh:
        data = fh.readlines()

    def docmd(cmd, x, y):
        if cmd == 'turn on':
            grid[x][y] = True
        elif cmd == 'turn off':
            grid[x][y] = False
        elif cmd == 'toggle':
            grid[x][y] = not grid[x][y]

    for line in data:
        cmd, (xA, yA), (xZ, yZ) = parse(line)
        for x in range(xA, xZ + 1):
            for y in range(yA, yZ + 1):
                docmd(cmd, x, y)

    print(sum(r.count(True) for r in grid))


if __name__ == '__main__':
    main()
