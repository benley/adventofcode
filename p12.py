#!/usr/bin/env python
"""JSAbacusFramework.io
Santa's Accounting-Elves need help balancing the books after a recent order.
Unfortunately, their accounting software uses a peculiar storage format.
That's where you come in.

They have a JSON document which contains a variety of things:

arrays ([1,2,3]),
objects ({"a":1, "b":2})
numbers
strings

Your first job is to simply find all of the numbers throughout the document
and add them together.

For example:

- [1,2,3] and {"a":2,"b":4} both have a sum of 6.
- [[[3]]] and {"a":{"b":4},"c":-1} both have a sum of 3.
- {"a":[-1,1]} and [-1,{"a":1}] both have a sum of 0.
- [] and {} both have a sum of 0.

You will not encounter any strings containing numbers.

What is the sum of all numbers in the document?
"""

import json
import re
import sys

def part1(data):
    numre = re.compile(r'-?\d+')
    return sum(int(x) for x in re.findall(numre, data))


def part2(jdata):
    """
    Uh oh - the Accounting-Elves have realized that they double-counted
    everything red.

    Ignore any object (and all of its children) which has any property with
    the value "red". Do this only for objects ({...}), not arrays ([...]).

    [1,2,3] still has a sum of 6.
    [1,{"c":"red","b":2},3] now has a sum of 4, because the middle object is
    ignored.
    {"d":"red","e":[1,2,3,4],"f":5} now has a sum of 0, because the entire
    structure is ignored.
    [1,"red",5] has a sum of 6, because "red" in an array has no effect.
    """
    if isinstance(jdata, int):
        return int(jdata)
    elif isinstance(jdata, basestring):
        return 0
    elif isinstance(jdata, list):
        return sum(part2(x) for x in jdata)
    elif isinstance(jdata, dict):
        if "red" in jdata.values():
            return 0
        return part2(jdata.keys()) + part2(jdata.values())
    else:
        raise RuntimeError("wtf? %s" % type(jdata))


def main():
    with open(sys.argv[1]) as ifh:
        data = ifh.read()

    print part1(data)
    print part2(json.loads(data))


if __name__ == '__main__':
    main()
