#!/usr/bin/env python3
import numpy as np

def adjacency_list_to_incidence_matrix(adj_list):
	node_count = len(adj_list)
	edge_count = sum(map(len, adj_list))
	dimensions_of_matrix = (node_count,node_count)
	inc_matrix = np.zeros(dimensions_of_matrix)
	for node_index in range(0, node_count):
		for neighbor_index in adj_list[node_index]:
			inc_matrix[node_index][neighbor_index] = 1
	return inc_matrix


def out_degree(M, u):
	l = len(M)
	count = 0
	for i in range (0,l):
		if M[u][i]==1:
			count=count+1
	return count

def in_degree(M, u):
	l = len(M)
	count = 0
	for i in range (0,l):
		if M[i][u]==1:
			count=count+1
	return count


#graph = [[0,1,0],[1,0,1],[1,1,0]]

graph = [[1],[0,2],[0,1]]
m = adjacency_list_to_incidence_matrix(graph)
print(m)
print("out degree of node 0 is " + str (out_degree(m,0)) )
print("in degree of node 0 is " + str (in_degree(m,0)) )
