#!/usr/bin/env python
"""One-line summary"""

import ast


def main():
    """Main"""
    with open('p8-input.txt') as ifh:
        data = ifh.readlines()

    total_literal = 0
    total_mem = 0
    total_repr = 0
    for line in data:
        line = line.strip()
        total_literal += len(line)
        total_mem += len(ast.literal_eval(line))

        new_repr = '"%s"' % line.encode("string_escape").replace('"', r'\"')
        print line
        print new_repr
        total_repr += len(new_repr)
    print total_literal - total_mem

    print total_repr - total_literal


if __name__ == '__main__':
    main()

