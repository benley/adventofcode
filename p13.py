#!/usr/bin/env python
"""One-line summary"""

import collections
import itertools
import re

PARSE_RE = re.compile(r"""(?x)
    (?P<person>\w+) \swould\s (?P<gainlose>gain|lose)\s
    (?P<points>\d+) \s happiness\s units\s by\s sitting\s next\s to\s
    (?P<otherperson>\w+).
    """)


def main():
    """Main"""
    with open("p13-input.txt") as ifh:
        data = ifh.readlines()

    weights = lambda: collections.defaultdict(int)
    peeps = collections.defaultdict(weights)

    for line in data:
        (person, losegain, units,
         otherperson) = re.match(PARSE_RE, line).groups()
        units = int(units)
        peeps[person][otherperson] = units if losegain == 'gain' else -units

    for person in peeps.keys():
        peeps["benley"][person] = 0
        peeps[person]["benley"] = 0

    maxhappiness = 0

    for ring in itertools.permutations(peeps):
        happiness = 0
        happiness += peeps[ring[0]][ring[1]]
        happiness += peeps[ring[0]][ring[-1]]
        happiness += peeps[ring[-1]][ring[0]]
        happiness += peeps[ring[-1]][ring[-2]]
        for pnum in range(1, len(ring)-1):
            happiness += peeps[ring[pnum]][ring[pnum+1]]
            happiness += peeps[ring[pnum]][ring[pnum-1]]

        if happiness > maxhappiness:
            maxhappiness = happiness

    print maxhappiness


if __name__ == '__main__':
    main()
