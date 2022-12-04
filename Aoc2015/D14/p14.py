#!/usr/bin/env python
"""Reindeer Olympics"""


def main():
    with open('p14-input.txt') as ifh:
        data = ifh.readlines()

    print part1(data)
    print part2(data)


def part1(data):
    duration = 2503
    alldeer = [Deer(line) for line in data]
    return winner({deer.name: deer.fly(duration) for deer in alldeer})


def winner(positions):
    board = sorted(positions.items(), key=lambda x: x[-1])
    name, position = board[-1]
    return name, position


def winners(scores):
    board = sorted(scores.items(), key=lambda x: x[-1])
    _, topscore = board[-1]
    return [name for name, score in scores.items() if score == topscore]


class Deer(object):
    def __init__(self, line):
        line = line.split()
        self.name = line[0]
        self.speed = int(line[3])
        self.flytime = int(line[6])
        self.resttime = int(line[-2])
        self.score = 0

    def fly(self, duration):
        period = self.flytime + self.resttime
        whole_cycles, extratime = duration // period, duration % period
        oneflight = self.speed * self.flytime

        if extratime > self.flytime:
            extra_distance = self.speed * self.flytime
        else:
            extra_distance = self.speed * extratime

        extra_distance = self.speed * min([self.flytime, extratime])
        distance = (whole_cycles * oneflight) + extra_distance
        return distance


def part2(data):
    alldeer = {line.split()[0]: Deer(line) for line in data}
    duration = 2503

    for tick in range(1, duration+1):
        positions = {deer.name: deer.fly(tick) for deer in alldeer.values()}

        for deer in winners(positions):
            alldeer[deer].score += 1

    return winner({deer.name: deer.score for deer in alldeer.values()})

if __name__ == '__main__':
    main()
