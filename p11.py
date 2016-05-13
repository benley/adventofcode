#!/usr/bin/env python
"""One-line summary"""

import sys


def rule1(pwd):
    """ Passwords must include one increasing straight of at least three
    letters, like abc, bcd, cde, and so on, up to xyz. They cannot skip letters
    abd doesn't count.
    """
    for letter in set(pwd):
        if letter.lower() in ['y', 'z']:
            continue
        ltr = ord(letter)
        ltrs = ''.join([chr(o) for o in range(ltr, ltr + 3)])
        if pwd.find(ltrs) > -1:
            return True
    return False

assert rule1("abc") is True
assert rule1("aaa") is False
assert rule1("lkkjshdfgkjhdsfdxyzjasdflk") is True


def rule2(pwd):
    """ Passwords may not contain the letters i, o, or l, as these letters can
    be mistaken for other characters and are therefore confusing.
    """
    for ltr in ['i', 'o', 'l']:
        if ltr in pwd:
            return False
    return True

assert rule2("asdfghjk") is True
assert rule2("asdfghjkilal") is False


def rule3(pwd):
    """ Passwords must contain at least two different, non-overlapping pairs
    of letters, like aa, bb, or zz.
    """
    matches = 0
    for ltr in set(pwd):
        if ltr * 2 in pwd:
            matches += 1
        if matches == 2:
            return True
    return False

assert rule3("aacbvnd") is False
assert rule3("aacbaa") is False
assert rule3("abbcdde") is True


def increment(pwd):
    ltr = pwd[-1]
    if ltr == "z":
        return increment(pwd[:-1]) + "a"
    return pwd[:-1] + chr(ord(ltr) + 1)

assert increment("asdf") == "asdg"
assert increment("asdz") == "asea"
assert increment("azzz") == "baaa"


def findnext(pwd):

    def findnext__(pwd):
        npwd = increment(pwd)
        if rule1(npwd) and rule2(npwd) and rule3(npwd):
            return (True, npwd)
        else:
            return (False, npwd)

    done, npwd = findnext__(pwd)
    while not done:
        done, npwd = findnext__(npwd)
    return npwd


def main():
    """Main"""
    print findnext(sys.argv[1])

if __name__ == '__main__':
    main()
