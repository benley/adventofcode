#!/usr/bin/env python
"""Fuck"""

from pyglib import app
from pyglib import log

import ctypes
import re


def parse(line):
    parse_re = re.compile(
        r"""
        (?x)
        (?: (?: (?P<id1> \w* ) \s* )?
            (?P<oper> AND|NOT|OR|RSHIFT|LSHIFT) \s+ )?
        (?P<id2> \w+ )
        \s+ -> \s+
        (?P<id3> \w+ )
        """)
    terms = re.match(parse_re, line).groupdict()
    expression = [x for x in (terms['id1'], terms['oper'], terms['id2'])
                  if x not in (None, '')]
    dest = terms['id3']
    return (expression, dest)


def evaluate(env, term):
    if isinstance(term, int):
        return term
    if isinstance(term, list) and len(term) == 1:
        term = term[0]

    if isinstance(term, basestring):
        try:
            ret = int(term)
        except ValueError:
            ret = evaluate(env, env[term])
            log.debug('%s => %s', term, ret)
        return ret
    if not isinstance(term, list):
        raise RuntimeError("wtf type %s" % term)

    elif len(term) == 2:
        op, expr = term
        if op == 'NOT':
            ret = ~ evaluate(env, expr)
            log.debug('%s %s => %s', op, expr, ret)
            return ret
        raise RuntimeError("Unknown operator: %s" % term[0])

    elif len(term) == 3:
        t1, t2 = term[0], term[2]
        p1 = evaluate(env, t1)
        env[t1] = p1
        op = term[1]
        p2 = evaluate(env, t2)
        env[t2] = p2
        if op == 'AND':
            ret = p1 & p2
        elif op == 'OR':
            ret = p1 | p2
        elif op == 'RSHIFT':
            ret = p1 >> p2
        elif op == 'LSHIFT':
            ret = p1 << p2
        else:
            raise RuntimeError("WTF? %s" % op)
        log.debug('%s %s %s => %s', p1, op, p2, ret)
        return ret

    raise RuntimeError("what the fuck is going on here: %s" % term)


def main(args):
    """Main"""
    filename = args[0]
    term = args[1]

    with open(filename) as ifh:
        data = ifh.readlines()

    env = {}

    for line in data:
        expr, dest = parse(line)
        if dest in env:
            raise RuntimeError('Tried to reassign %s' % dest)
        env[dest] = expr

    print ctypes.c_uint16(evaluate(env, [term]))


if __name__ == '__main__':
    app.run()
