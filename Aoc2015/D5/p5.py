#!/usr/bin/env python
import re


def has3vowels(line):
    line = line.lower()
    vowels = 0
    for char in "aeiou":
        vowels += line.count(char)
        if vowels >= 3:
            return True


def doubleletter(line):
    for char in set(line):
        if line.count(char*2):
            return True


def badstrings(line):
    bads = ["ab", "cd", "pq", "xy"]
    for bad in bads:
        if line.count(bad):
            return True


def part1(idata):
    nice = 0

    for line in idata:
        if has3vowels(line) and doubleletter(line) and not badstrings(line):
            nice += 1
    print("Part 1:", nice)


def noverlapxx(line):
    pos = 0
    while pos < len(line):
        pair = line[pos:pos+2]
        if line.count(pair) > 1:
            return True
        pos += 1


def a_b_a(line):
    for char in set(line):
        d_re = r'%s.%s' % (char, char)
        if re.search(d_re, line):
            return True


def part2(idata):
    nice = 0
    for line in idata:
        if noverlapxx(line) and a_b_a(line):
            nice += 1
    print("Part 2:", nice)


def main():
    """Main"""
    idata = open("p5_input.txt").readlines()

    part1(idata)
    part2(idata)


if __name__ == '__main__':
    main()
