#include <iostream>
#include <iomanip>
#include <fstream>

 
using namespace std;

//the queue implementation was taken from
//https://www.tutorialspoint.com/cplusplus-program-to-implement-queue-using-linked-list?fbclid=IwAR0ZMiOMgCbBOOqLbTo3ZBpVmedgdoOz1a6jyMzMe4tBc8tH4Z4tAqxYugc
//and modified to meet the problem's requirements

struct pos {
	int x, y, time=-2, t=-2, d;
	bool visited = false;
	string path="";
} A[1000000];

struct node {
   pos data;
   struct node *next;
};

struct node* front = NULL;
struct node* rear = NULL;
struct node* temp;

void Insert(pos val) {
   if (rear == NULL) {
      rear = new node;
      rear->next = NULL;
      rear->data = val;
      front = rear;
   } else {
      temp=new node;
      rear->next = temp;
      temp->data = val;
      temp->next = NULL;
      rear = temp;
   }
}

bool isEmpty()
{
	return front==NULL;
}

pos Delete() {
   pos a;
   temp = front;
   if (temp->next != NULL) {
      temp = temp->next;
      a = front->data;
      delete front;
      front = temp;
   } else {
      a = front->data;
      delete front;
      front = NULL;
      rear = NULL;
   }
   return a;
}
void Display() {
   temp = front;
   if ((front == NULL) && (rear == NULL)) {
      cout<<"Queue is empty"<<endl;
      return;
   }
   cout<<"Queue elements are: "<<endl;
   while (temp != NULL) {
      cout<<temp->data.x<<" "<<temp->data.y<<" "<<temp->data.d<<endl;
      temp = temp->next;
   }
   cout<<endl;
}




int main(int argc, char** argv)
{
	ifstream myfile;
	myfile.open(argv[1]);
	int N=0, M=0, j=0;
	pos cat, a;
	char c;
	
	while(myfile.get(c))
	{
		if(myfile.eof()) break;
		if (c == '\n')
		{
			N++;
			M = j;
			j=0;
		}
		else
		{
			a.x=N;
			a.y=j;
			if(c=='.')
			{
		       	a.d=-1;
				A[N*M+j]=a;
			}
			else if(c=='X')
			{
				a.d=-9;
				A[N*M+j]=a;
			}
			else if(c=='W')
			{
				a.d=-1;
				a.time=0;
				A[N*M+j]=a;
				Insert(a);
			}
			else if(c=='A')
			{
				cat.x=N;
				cat.y=j;
				cat.d=-1;
				cat.t=0;
				a.d=-1;
				A[N*M+j]=a;
			}
			j++;
		}
	}
	myfile.close();
	N++;


	while(!isEmpty())
	{
		pos a=Delete();
		if(A[a.x*M+a.y].d==-1)
		{
			A[a.x*M+a.y].d=a.time;
			a.time++;
			if(a.x>0)
			{
				a.x--;
				if(A[a.x*M+a.y].d!=-9) Insert(a);
				a.x++;
			}
			if(a.x<N-1)
			{
				a.x++;
				if(A[a.x*M+a.y].d!=-9) Insert(a);
				a.x--;
			}
			if(a.y>0)
			{
				a.y--;
				if(A[a.x*M+a.y].d!=-9) Insert(a);
				a.y++;
			}
			if(a.y<M-1)
			{
				a.y++;
				if(A[a.x*M+a.y].d!=-9) Insert(a);
				a.y--;
			}
		}
	}

	
	int max=-2, safe;
	
	cat=A[cat.x*M+cat.y];
	cat.t=0;
	
	pos cat_new=cat;
	A[cat.x*M+cat.y].visited=true;
	Insert(cat);


	while(!isEmpty())
	{
		pos a=Delete();
		safe=A[a.x*M+a.y].d-1;
		if(safe>max)
		{
			max=safe;
			cat_new=a;
		}
		else if(safe==max)stringbuilder
		{
			if(a.x<cat_new.x || (a.x==cat_new.x && a.y<cat_new.y)) cat_new=a;
		}
		a.t++;
		if(a.x<N-1)
		{
			a.x++;
			if(A[a.x*M+a.y].d>a.t && !A[a.x*M+a.y].visited)
			{
				A[a.x*M+a.y].visited=true;
				A[a.x*M+a.y].path=A[(a.x-1)*M+a.y].path+"D";
				Insert(a);
			}
			a.x--;
		}
		if(a.y>0)
		{
			a.y--;
			if((A[a.x*M+a.y].d>a.t || A[a.x*M+a.y].d==-1) && !A[a.x*M+a.y].visited)
			{
				A[a.x*M+a.y].visited=true;
				A[a.x*M+a.y].path=A[a.x*M+(a.y+1)].path+"L";
				Insert(a);
			}
			a.y++;
		}
		if(a.y<M-1)
		{
			a.y++;
			if((A[a.x*M+a.y].d>a.t || A[a.x*M+a.y].d==-1) && !A[a.x*M+a.y].visited)
			{
				A[a.x*M+a.y].visited=true;
				A[a.x*M+a.y].path=A[a.x*M+(a.y-1)].path+"R";
				Insert(a);
			}
			a.y--;
		}
		if(a.x>0)
		{
			a.x--;
			if((A[a.x*M+a.y].d>a.t || A[a.x*M+a.y].d==-1) && !A[a.x*M+a.y].visited)
			{
				A[a.x*M+a.y].visited=true;
				A[a.x*M+a.y].path=A[(a.x+1)*M+a.y].path+"U";
				Insert(a);
			}
			a.x++;
		}
	}
	
	if(A[cat_new.x*M+cat_new.y].d==-1) cout<<"infinity"<<endl;
	else cout<<A[cat_new.x*M+cat_new.y].d-1<<endl;
	if(cat_new.x==cat.x && cat_new.y==cat.y) cout<<"stay"<<endl;
	else cout<<A[cat_new.x*M+cat_new.y].path<<endl;
	

  
	return 0;
}
