#!/usr/bin/env python3

def f(a):
	a = list(a)
	s = sum(a)
	return [x / s for x in a]

print(f(n*n for n in range(4)))
