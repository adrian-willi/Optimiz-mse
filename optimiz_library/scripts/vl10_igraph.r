m = matrix(0, 6, 6)
m[1,2] = m[2,1] = 2
m[1,4] = m[4,1] = 1
m[2,3] = m[3,2] = 7
m[2,4] = m[4,2] = 3
m[2,5] = m[5,2] = 5
m[3,6] = m[6,3] = 6
m[4,5] = m[5,4] = 4
m[5,6] = m[6,5] = 8

library(igraph)
g  = graph.adjacency(m, mode="undirected", weighted=T)
plot(g, edge.label=E(g)$weight)




n=100
m = matrix(rbinom(n*n,1,.08), n, n)
diag(m)<-0
library(igraph)
g  = graph.adjacency(m, mode="directed", weighted=T)
plot(g,vertex.size=4,edge.color="dark gray")

