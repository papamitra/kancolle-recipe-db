#!/usr/bin/env python
# -*- coding: utf-8 -*-

# curl view-source:http://www56.atwiki.jp/kancolle/pages/24.html | python equipparse.py

import sys
import re

re_str = r"<!--(\d+)-(\d+)--><td.*?>(?:<a.*?>)?(.*?)(?:</a>)?</td>"
equips = {}
equiptypes = {}

NAME=2
LIBNO=0
TYPE=3
for line in sys.stdin:
    m = re.search(re_str, line)
    if m:
        row_no = int(m.group(1))
        key = int(m.group(2))
        value = m.group(3)
        if not equips.has_key(row_no):
            equips[row_no] = {}

        equips[row_no][key] = value

for (row_no, row) in equips.items():
    if (not row.has_key(1)) or row[NAME]=="" or row[NAME]=="装備名" :
        del equips[row_no]
    
max_no = 1
equipcsv = {}
max_equipno = 0

#ペンギンは最初に出力しとこ
print "insert or ignore into 'equipment_class' (name) values ('その他');"
print "insert or ignore into 'equipment' (lib_no, name, type) select -1, 'ペンギン', equipment_class.id from equipment_class where equipment_class.name == 'その他';"

try:
    with open("equips.csv", "r") as f:
        for line in f.readlines():
            (id, name) = line.rstrip().split(",")
            equipcsv[name] = int(id)
    if len(equipcsv) > 0:
      max_equipno = max(equipcsv.values()) + 1
except IOError as (errno, strerror):
    print >> sys.stderr, strerror

for (row, equip) in sorted(equips.items()):
    if equipcsv.has_key(equip[NAME]):
        equipid = equipcsv[equip[NAME]]
    else:
        equipid = max_equipno
        equipcsv[equip[NAME]] = equipid
        max_equipno += 1

    print "insert or ignore into 'equipment_class' (name) values ('%s');" % (equip[TYPE])
    print "insert or ignore into 'equipment' (lib_no, name, type) select %s, '%s', equipment_class.id from equipment_class where equipment_class.name == '%s';" % (equip[LIBNO], equip[NAME], equip[TYPE])

with open("equips.csv", "w") as f:
    for (id, name) in sorted([(id, name) for (name , id) in equipcsv.items()]):
        print >> f, "%d,%s" % (id, name)

