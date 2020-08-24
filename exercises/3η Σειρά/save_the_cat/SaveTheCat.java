import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList; 
import java.util.Queue; 
import java.util.List;
//import pos.java;


//http://www.java2s.com/Code/Java/File-Input-Output/Readfilecharacterbycharacter.htm
 class pos {
	int x,y;
	int cat_time=0;
	int bfs_flood=-1;
	int time = -1;
	char sign;
	boolean visited = true;
	String path="";
	
	public pos (int x, int y, char sign){
		this.x = x;
		this.y = y;
		this.sign = sign;
	}
	public pos(){}
	public pos (pos a){
		this.x=a.x;
		this.y=a.y;
		this.time=a.time;
		this.sign=a.sign;
		this.visited=a.visited;
		this.path=a.path;
	}
		
	public String toString(){
		return this.bfs_flood+" ";
	}
}
public class SaveTheCat{
	public static void main (String [] args){
		int N=0,M=0,i=0;
		List<pos> A = new ArrayList<pos>();
		Queue<pos> queue = new LinkedList<>();
		pos cat = new pos();

		File file = new File(args[0]);
    		if (!file.exists()) {
      			System.out.println(args[0] + " does not exist.");
      			return;
    		}
    		if (!(file.isFile() && file.canRead())) {
      			System.out.println(file.getName() + " cannot be read from.");
      			return;
    		}
    		try {
      			FileInputStream fis = new FileInputStream(file);
      			char current;
      			while (fis.available() > 0) {
        			current = (char) fis.read();
        			if(current == '\n'){
					N++;
					M=i;
					i=0;
				}
				else{

					if(current == '.'){
						pos a = new pos(N,i,'.');
						A.add(N*M + i, a);
					}
					else if(current == 'W'){
						pos a = new pos(N,i,'W');
						A.add(N*M + i, a);
 						queue.add(a);
					}
					else if(current == 'X'){
						pos a = new pos(N,i,'X');
						a.time=-9;
						A.add(N*M + i,a);
					}
					else if(current == 'A'){
						cat = new pos(N,i,'A');
						pos a=cat;
						A.add(N*M + i,a);						
					}
					i++;
      				}
    			}
		}
		 catch (IOException e) {
  			e.printStackTrace();
		}
		

		pos a,b;
		int time;
		while(queue.size()!=0){
		a = queue.poll();
		if(A.get(a.x*M + a.y).time==-1){
			time = A.get(a.x*M + a.y).bfs_flood;
			time++;
			A.get(a.x*M + a.y).time = time;	
			if(A.get(a.x*M + a.y).x>0)
				if(A.get((a.x-1)*M + a.y).sign != 'X'){
					b = A.get((a.x-1)*M + a.y);
					b.bfs_flood = time;
					queue.add(b);
				}
			if(A.get(a.x*M + a.y).x<N-1)
				if(A.get((a.x+1)*M + a.y).sign != 'X'){
					b = A.get((a.x+1)*M + a.y);
					b.bfs_flood = time;
					queue.add(b);
				}
			if(A.get(a.x*M + a.y).y>0)
				if(A.get(a.x*M + a.y-1).sign != 'X'){
					b = A.get(a.x*M + a.y-1);
					b.bfs_flood = time;
					queue.add(b);
				}
			if(A.get(a.x*M + a.y).y<M-1)
				if(A.get(a.x*M + a.y+1).sign != 'X'){
					b = A.get(a.x*M + a.y+1);
					b.bfs_flood = time;
					queue.add(b);
				}

	

		}
		}
		/*System.out.println(N +" " +M);
		for(i=0; i<A.size(); i++){
			if(i%M==0) System.out.println();
			if(A.get(i).time==-1) System.out.print(" X "); 
			else System.out.print(A.get(i));		
		}System.out.println();*/
		queue.add(cat);
		int safe_time;
		int max_time = -2;
		int cat_time;
		pos original_cat = new pos(cat);
		while(queue.size()!=0){
			pos cat_new = queue.poll();
			safe_time = A.get(cat_new.x*M + cat_new.y).time-1;
			if(safe_time>max_time){
				max_time=safe_time;
				cat = cat_new;
			}
			else if(safe_time==max_time)
				if(cat_new.x<cat.x || cat_new.x==cat.x && cat_new.y<cat.y)
					cat = cat_new;
			cat_time=cat_new.cat_time+1;
			if(cat_new.x<N-1){
				cat_new.x++;
				if(A.get(cat_new.x*M + cat_new.y).time>cat_new.cat_time && A.get(cat_new.x*M + cat_new.y).visited){
					A.get(cat_new.x*M + cat_new.y).cat_time=cat_time;
					A.get(cat_new.x*M + cat_new.y).visited = false;
					A.get(cat_new.x*M + cat_new.y).path = A.get((cat_new.x-1)*M + cat_new.y).path + "D";
					queue.add(A.get(cat_new.x*M + cat_new.y));
				}
				cat_new.x--;
			}
			if(cat_new.y>0){
				cat_new.y--;
				if((A.get(cat_new.x*M + cat_new.y).time>cat_new.cat_time ||A.get(cat_new.x*M + cat_new.y).time==-1) && A.get(cat_new.x*M + cat_new.y).visited){
					A.get(cat_new.x*M + cat_new.y).cat_time=cat_time;
					A.get(cat_new.x*M + cat_new.y).visited = false;
					A.get(cat_new.x*M + cat_new.y).path = A.get(cat_new.x*M + cat_new.y+1).path+"L";
					queue.add(A.get(cat_new.x*M + cat_new.y));
				}
				cat_new.y++;
			}
			if(cat_new.y<M-1){
				cat_new.y++;
				if((A.get(cat_new.x*M + cat_new.y).time>cat_new.cat_time ||A.get(cat_new.x*M + cat_new.y).time==-1) && A.get(cat_new.x*M + cat_new.y).visited){
					A.get(cat_new.x*M + cat_new.y).cat_time=cat_time;
					A.get(cat_new.x*M + cat_new.y).visited = false;
					A.get(cat_new.x*M + cat_new.y).path = A.get(cat_new.x*M + cat_new.y-1).path+"R";
					queue.add(A.get(cat_new.x*M + cat_new.y));
				}
				cat_new.y--;
			}
			if(cat_new.x>0){
				cat_new.x--;
				if((A.get(cat_new.x*M + cat_new.y).time>cat_new.cat_time ||A.get(cat_new.x*M + cat_new.y).time==-1) && A.get(cat_new.x*M + cat_new.y).visited){
					A.get(cat_new.x*M + cat_new.y).cat_time=cat_time;
					A.get(cat_new.x*M + cat_new.y).visited = false;
					A.get(cat_new.x*M + cat_new.y).path = A.get((cat_new.x+1)*M + cat_new.y).path+"U";
					queue.add(A.get(cat_new.x*M + cat_new.y));
				}
				cat_new.x++;
			}

		}
		if(A.get(cat.x*M + cat.y).time==-1) System.out.println("infinity");
		else System.out.println(A.get(cat.x*M + cat.y).time-1);
		if(original_cat.x==cat.x && original_cat.y==cat.y) System.out.println("stay");
		else System.out.println(A.get(cat.x*M + cat.y).path);
		
	}
}


