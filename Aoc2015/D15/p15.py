#!/usr/bin/env python
"""One-line summary"""

import itertools


def main():
    """Main"""
    with open('p15-input.txt') as ifh:
        data = ifh.readlines()

    ingredients = {}
    for line in data:
        name, rest = [x.strip() for x in line.split(":")]
        props = {k: int(v) for k, v in (x.split() for x in rest.split(','))}
        ingredients[name] = props

    props = ['capacity', 'durability', 'flavor', 'texture']

    maxscore = 0
    for combo in itertools.combinations_with_replacement(ingredients, 100):
        counts = {thing: combo.count(thing) for thing in ingredients}

        subscores = []
        for prop in props:
            propscores = []

            for thing, num in counts.items():
                val = ingredients[thing][prop]
                # print num, thing, prop, val
                propscores.append(val*num)
            subscores.append(max([sum(propscores), 0]))
        score = reduce(int.__mul__, subscores)
        if score > max:
            maxscore = score
    print(maxscore)


if __name__ == '__main__':
    main()
