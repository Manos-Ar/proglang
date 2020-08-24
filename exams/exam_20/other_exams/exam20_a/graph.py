#!/usr/bin/env python3
import numpy

def has_edge_mat(M,u,v) :
	if M[u][v]==1:  
		return True
	return False

def adj_mat_list(M):
	V = len(M)
	
	adj = []
	for i in range(0,V):
		adj.append([])
		for j in range(0,V):
			if M[i][j]==1:
				adj[i].append(j)
	return adj

def has_edge_list(G,u,v):
	edges = len(G[u])
	for i in range(0,edges):
		if G[u][i]==v :
			return True
	return False
		
M = [[0,1,0,0,1],[1,0,1,1,1],[0,1,0,1,0],[0,1,1,0,1],[1,1,0,1,0]]

print(has_edge_mat(M,4,3))
adj=adj_mat_list(M)
print(adj)
print(has_edge_list(adj,0,1))
