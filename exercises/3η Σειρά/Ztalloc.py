from collections import deque
import sys

class ZtallocState(object):
	def __init__(self, l, r, path):
		self.l=l
		self.r=r
		self.path=path

	def get_l(self):
		return self.l

	def get_r(self):
		return self.r

	def get_path(self):
		return self.path

	def isFinal(self, tl, tr):
		if self.l>=tl and self.r<= tr:
			return True
		else:
			return False

	def isBad(self):
		if self.r>999999:
			return True
		else:
			return False

	def next(self):
		a=[]
		a.append(ZtallocState(self.l//2, self.r//2, self.path+"h"))
		a.append(ZtallocState(self.l*3+1, self.r*3+1, self.path+"t"))
		return a

	def __eq__(self, other):
		if isinstance(other,ZtallocState):
			return self.l == other.l and self.r == other.r
		else:
			return False

	def __ne__(self, other):
		return not self.__eq__(other)

	def __str__(self):
		return str(self.l)+" "+str(self.r)+" "+self.path

	def __repr__(self):
		return str(self.l)+" "+str(self.r)+" "+self.path

	def __hash__(self):
		a = self.l+self.r
		return (a)*(a+1)//2+self.l


def ZtallocSolver(initial, l, r):
	remaining = deque([])
	seen = dict()
	seen[hash(initial)]=1
	remaining.append(initial)
	while remaining:
		a = remaining.popleft()
		if a.isFinal(l, r):
			return a
		b = a.next()
		for i in range(2):
			c = b[i]
			if not c.isBad() and not hash(c) in seen:
				remaining.append(c)
				seen[hash(c)]=1
	return None

def Ztalloc(l, r, tl, tr):
	a = ZtallocSolver(ZtallocState(l, r, ""), tl, tr)
	if a is None:
		print("IMPOSSIBLE")
	elif a.get_path()=="":
		print("EMPTY")
	else:
		print(a.get_path())

with open(sys.argv[1]) as f:
	a = [int(x) for x in next(f).split()]
	a=a[0]
	for i in range(a):
		b = [int(x) for x in next(f).split()]
		Ztalloc(b[0], b[1], b[2], b[3])
f.close()