#!/usr/bin/env python

import sys
import re

def main():
    lines = []
    for line in sys.stdin:
        m = re.match("^[^;]*", line)
        line = m.group().strip()
        if line:
            lines.append(line)

    symtab = {} # map label to addr
    addr = 0
    for line in lines:
        if line.endswith(":"):
            label = line[:-1]
            symtab[label] = addr
        else:
            addr += 1

    for line in lines:
        if not line.endswith(":"):
            line = re.sub("[A-Z]+", lambda m: str(symtab[m.group()]), line)
            print line

main()
