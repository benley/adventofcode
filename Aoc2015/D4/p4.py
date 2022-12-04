#!/usr/bin/env python
"""One-line summary"""

import hashlib


def main():
    """Main"""
    data = "ckczppom"
    num = 0

    while True:
        dhash = hashlib.md5(data + str(num)).hexdigest()
        if dhash.startswith("000000"):
            print(num)
            return
        num += 1


if __name__ == '__main__':
    main()
