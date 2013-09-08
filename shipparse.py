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

max_shipno = 1
shipcsv = {}

try:
    with open("ships.csv", "r") as f:
        for line in f.readlines():
            (id, name) = line.rstrip().split(",")
            shipcsv[name] = int(id)

except IOError as (errno, strerror):
    print >> sys.stderr, strerror


for (row, ship) in sorted(ships.items()):
    if shipcsv.has_key(ship[NAME]):
        shipid = shipcsv[ship[NAME]]
    else:
        shipid = max_shipno
        shipcsv[ship[NAME]] = shipid
        max_shipno += 1

    print "insert into 'ship' values (%d, %s, '%s');" % (shipid, ship[LIBNO], ship[NAME])

with open("ships.csv", "w") as f:
    for (id, name) in sorted([(id, name) for (name , id) in shipcsv.items()]):
        print >> f, "%d,%s" % (id, name)

