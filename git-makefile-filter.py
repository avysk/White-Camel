#!/usr/bin/env python2
import re
import sys

in_oasis = False

for line in sys.stdin:
    if in_oasis:
        if re.match("^# OASIS_STOP.*", line):
            in_oasis = False
            print line,
    else:
        if re.match("^# OASIS_START.*", line):
            in_oasis = True
        print line,

