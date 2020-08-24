from collections import deque
import sys


file = open(sys.argv[1], "rt")
a=[]
N=1
M=0
x=0
y=0
flag=False
queue = deque([])
while True :
	c = file.read(1)
	if not c:
		N=N-1
		break
	if c=="\n":
		flag=True
	if flag==False: 
		M=M+1
	if c=="\n":
		N=N+1	
		x=x+1
	elif c=="W":
		a.append([x%N,y%M,-1,True,"","w"])
		queue.append((x%N,y%M,-1))
		y=y+1
	elif c=="A":
		a.append([x%N,y%M,-1,True,"","a"])
		cat=[x%N,y%M,0]
		y=y+1
	elif c==".":
		a.append([x%N,y%M,-1,True,"","."])
		y=y+1
	else:
		a.append([x%N,y%M,-9,True,"","x"])
		y=y+1

while True:
	if queue:
		value = queue.popleft()
		if a[value[0]*M + value[1]][2]==-1:		
			x=value[0]
			y=value[1]
			t=value[2]
			t=t+1
			a[x*M + y][2]=t
			if x>0:
				x=x-1
				if a[x*M+y][5]!="x":
					queue.append((x,y,t))
				x=x+1
			if x<N-1:
				x=x+1
				if a[x*M+y][5]!="x":
					queue.append((x,y,t))
				x=x-1
			if y>0:
				y=y-1			
				if a[x*M+y][5]!="x":
					queue.append((x,y,t))
				y=y+1
			if y<M-1:
				y=y+1			
				if a[x*M+y][5]!="x":
					queue.append((x,y,t))
				y=y-1
	else:
		break



queue.append([cat[0],cat[1],0])
max_time=-2
b=cat
while queue:
	cat_new=queue.popleft()
	safe_time=a[cat_new[0]*M+cat_new[1]][2]-1
	if(safe_time>max_time):
		max_time=safe_time
		cat=cat_new
	elif(safe_time==max_time):
		if(cat_new[0]<cat[0] or (cat_new[0]==cat[0] and cat_new[1]<cat[1])):
			cat=cat_new
	cat_new[2]=cat_new[2]+1
	if(cat_new[0]<N-1):
		cat_new[0]=cat_new[0]+1
		if(a[cat_new[0]*M+cat_new[1]][2]>cat_new[2] and a[cat_new[0]*M+cat_new[1]][3]):
			a[cat_new[0]*M+cat_new[1]][3]=False
			a[cat_new[0]*M+cat_new[1]][4]=a[(cat_new[0]-1)*M+cat_new[1]][4]+"D"
			queue.append([cat_new[0],cat_new[1],cat_new[2]])
		cat_new[0]=cat_new[0]-1
	if(cat_new[1]>0):
		cat_new[1]=cat_new[1]-1
		if((a[cat_new[0]*M+cat_new[1]][2]>cat_new[2] or a[cat_new[0]*M+cat_new[1]][2]==-1) and a[cat_new[0]*M+cat_new[1]][3]):
			a[cat_new[0]*M+cat_new[1]][3]=False
			a[cat_new[0]*M+cat_new[1]][4]=a[(cat_new[0])*M+cat_new[1]+1][4]+"L"
			queue.append([cat_new[0],cat_new[1],cat_new[2]])
		cat_new[1]=cat_new[1]+1
	if(cat_new[1]<M-1):
		cat_new[1]=cat_new[1]+1
		if((a[cat_new[0]*M+cat_new[1]][2]>cat_new[2] or a[cat_new[0]*M+cat_new[1]][2]==-1) and a[cat_new[0]*M+cat_new[1]][3]):
			a[cat_new[0]*M+cat_new[1]][3]=False
			a[cat_new[0]*M+cat_new[1]][4]=a[(cat_new[0])*M+cat_new[1]-1][4]+"R"
			queue.append([cat_new[0],cat_new[1],cat_new[2]])
		cat_new[1]=cat_new[1]-1
	if(cat_new[0]>0):
		cat_new[0]=cat_new[0]-1
		if((a[cat_new[0]*M+cat_new[1]][2]>cat_new[2] or a[cat_new[0]*M+cat_new[1]][2]==-1) and a[cat_new[0]*M+cat_new[1]][3]):
			a[cat_new[0]*M+cat_new[1]][3]=False
			a[cat_new[0]*M+cat_new[1]][4]=a[(cat_new[0]+1)*M+cat_new[1]][4]+"U"
			queue.append([cat_new[0],cat_new[1],cat_new[2]])
		cat_new[0]=cat_new[0]+1

if(a[cat[0]*M+cat[1]][2]==-1):
	print("infinity")
else:
	print(a[cat[0]*M+cat[1]][2]-1)
if(b[0]==cat[0] and b[1]==cat[1]):
	print("stay")
else:
	print(a[cat[0]*M+cat[1]][4])

