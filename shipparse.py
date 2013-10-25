#!/usr/bin/env python
# -*- coding: utf-8 -*-

# curl view-source:http://www56.atwiki.jp/kancolle/pages/18.html | python shipparse.py

import sys
import re

re_str = r"<!--(\d+)-(\d+)--><td.*?>(?:<a.*?>)?(.*?)(?:</a>)?</td>"
ships = {}

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
    max_shipno = max(shipcsv.values()) + 1
except IOError as (errno, strerror):
    print >> sys.stderr, strerror

NAME=1
LIBNO=0
CLASS=2
TYPE=3
GET=21
for (row, ship) in sorted(ships.items()):
    if shipcsv.has_key(ship[NAME]):
        shipid = shipcsv[ship[NAME]]
    else:
        shipid = max_shipno
        shipcsv[ship[NAME]] = shipid
        max_shipno += 1

    buildable = 1 if re.search("建造", ship[GET]) else 0
        
    print "insert or ignore into 'ship_types' (name) values ('%s');" % (ship[TYPE])
    print "insert or ignore into 'ship' (id, lib_no, name, buildable, type) select %s, %s, '%s', %d, ship_types.id from ship_types where ship_types.name == '%s';" % (shipid, ship[LIBNO], ship[NAME], buildable, ship[TYPE])

REMODEL=4
for (row, ship) in sorted(ships.items()):
    if (not ship.has_key(REMODEL)) or ship[REMODEL] == "": continue
    m = re.search(r"\d+/(?:<a.*?>)?(.*?)(?:</a>)?$", ship[REMODEL])
    if m:
        remodelname = m.group(1)
        if remodelname in [s[NAME] for (row, s)in ships.items()]: continue
        if not shipcsv.has_key(remodelname):
            print "insert or ignore into 'ship' (id, lib_no, name, buildable, type) select %d, %s, '%s', 0, ship.type from ship where ship.name == '%s';" % (max_shipno, -1, remodelname, ship[NAME])
            shipcsv[remodelname] = max_shipno
            max_shipno += 1
        else:
            print "insert or ignore into 'ship' (id, lib_no, name, buildable, type) select %d, %s, '%s', 0, ship.type from ship where ship.name == '%s';" % (shipcsv[remodelname], -1, remodelname, ship[NAME])

with open("ships.csv", "w") as f:
    for (id, name) in sorted([(id, name) for (name , id) in shipcsv.items()]):
        print >> f, "%d,%s" % (id, name)

