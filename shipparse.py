#!/usr/bin/env python
# -*- coding: utf-8 -*-

# curl view-source:http://www56.atwiki.jp/kancolle/pages/18.html | python shipparse.py

import sys
import re

re_str = r"<!--(\d+)-(\d+)--><td.*?>(?:<a.*?>)?(.*?)(?:</a>)?</td>"
ships = {}

NAME=1
LIBNO=0
for line in sys.stdin:
    m = re.search(re_str, line)
    if m:
        row = int(m.group(1))
        key = int(m.group(2))
        value = m.group(3)
        if not ships.has_key(row):
            ships[row] = {}

        ships[row][key] = value

for (row, ship) in ships.items():
    if (not ship.has_key(1)) or ship[1]=="" or ship[1]=="艦名" :
        del ships[row]

shipid = 1
for (row, ship) in ships.items():
    print "insert into 'ship' values (%d, %s, '%s');" % (shipid, ship[LIBNO], ship[NAME])
    shipid += 1

