#!/usr/bin/env python3
#from sets import Set
import sys

def count_substr(strg, val):
	lenght = len(strg)
	result = set()
	ival = int(val)
	if (lenght < ival) :
		return 0
	for i in range(0,lenght):
		if i+ival-1 < lenght: 
			result.add(strg[i:i+ival])
	return len(result)
	
print (count_substr(sys.argv[1],sys.argv[2]))
