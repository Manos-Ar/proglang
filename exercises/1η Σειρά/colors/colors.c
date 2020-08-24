#include <stdio.h>

int main(int argc, char **argv)
{
	FILE *fp;
	fp = fopen(argv[1], "rt");
	int N, k, colors=0, i=0, j=0, min;
	fscanf(fp, "%d %d", &N, &k);
	int A[N], B[k+1];
	for(int a=0; a<N; a++) fscanf(fp, "%d", &A[a]);
	fclose(fp);
	for(int a=0; a<=k; a++) B[a]=0;
	min=N+1;
	while(colors!=k)
	{
		B[A[j]]++;
		j++;
		if(B[A[j-1]]==1) colors++;
		if(j==N && colors!=k)
		{
			printf("%d\n", 0);
			return 0;
		}
	}
	min=j-i;
	while(1)
	{
		for(;; i++)
		{
			if(B[A[i]]==1)
			{
				if(j-i < min) min=j-i;
				B[A[i]]--;
				break;
			}
			B[A[i]]--;
		}
		i++;
		if(j==N) break;
		for(; j<N; j++)
		{
			if(B[A[j]]==0)
			{
				B[A[j]]++;
				j++;
				break;
			}
			B[A[j]]++;
		}	
	}
	printf("%d\n", min);
	return 0;
}
