#!/usr/bin/python3

import sys
import re

for sid, line in enumerate(sys.stdin.readlines()):
    ls = line.split(" ")
    items = []
    eid = 0
    for s in ls:
        s = s.strip()
        m = re.match(r'<(\d+)>', s)
        if m:
            eid = int(m.group(1))
        elif s == '-1':
            print(sid, eid, *items)
            items.clear()
            eid += 1
        elif s == '-2':
            continue
        else:
            items.append(s)
