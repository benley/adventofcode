#!/usr/bin/env python


def part1(data):
    acc = 0
    for line in data:
        (x, y, z) = [int(i) for i in line.split("x")]
        s1 = x * y
        s2 = x * z
        s3 = y * z
        slack = min([s1, s2, s3])
        acc += slack + (2 * (s1 + s2 + s3))
    print "part 1: %s" % acc


def part2(data):
    acc = 0
    for line in data:
        (x, y, z) = [int(i) for i in line.split("x")]
        p1 = 2 * (x + y)
        p2 = 2 * (x + z)
        p3 = 2 * (y + z)
        perim = min([p1, p2, p3])
        vol = x * y * z
        acc += perim + vol
    print "part 2: %s" % acc


def main():
    """Main"""
    data = open("p2_input.txt").readlines()
    part1(data)
    part2(data)


if __name__ == '__main__':
    main()
