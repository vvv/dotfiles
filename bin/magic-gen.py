#!/usr/bin/env python

from random import shuffle
import re
from string import maketrans

## Length of mnemonics.
##   magic value = 64 bits
##   reserved = 16 bits
##   64 - 16 = 48 bits = 12 nibbles
nr_nibbles = 12

def words():
    hexspeakable = re.compile('^[a-foilsz]+$')
    ws = filter(lambda s: len(s) <= nr_nibbles and re.match(hexspeakable, s),
                [w[:-1].lower() for w in file('/usr/share/dict/words')])
    shuffle(ws)
    return ws

def magic_gen(ws):
    tr = maketrans('oilsz', '01152')

    while ws:
        acc = [('', '')] * 1000
        while acc and ws:
            w = ws.pop(0)

            for i, (magic, mnemonics) in enumerate(acc):
                assert len(magic) < nr_nibbles

                n = len(magic + w)
                if n < nr_nibbles:
                    acc[i] = (magic + w.translate(tr), mnemonics + ' ' + w)
                    break
                elif n == nr_nibbles:
                    del acc[i]
                    print magic + w.translate(tr) + mnemonics + ' ' + w
                    break

if __name__ == '__main__':
    magic_gen(words())
