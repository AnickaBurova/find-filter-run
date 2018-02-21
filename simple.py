#!/bin/env python2

import sys

data = list(map(ord, sys.stdin.read()))
if len(data) == 12:
    print ("data = {} {}".format(len(data),data))
elif len(data) == 7:
    print("this is my special case")
else:
    print("I dont want it")

