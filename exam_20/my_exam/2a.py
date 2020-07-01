def f(a):
	a = list(a)
	s = sum(a)
	return [x / s for x in a]
