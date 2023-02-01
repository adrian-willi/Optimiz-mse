# R-package to plot and work with graphs
library(igraph)

# example 1: adjacency matrix of undirected graph
m = matrix(0, 8, 8)
m[1,2] = m[2,1] = 1
m[1,4] = m[4,1] = 1
m[2,4] = m[4,2] = 1
m[2,5] = m[5,2] = 1
m[3,4] = m[4,3] = 1
m[4,6] = m[6,4] = 1
m[4,8] = m[8,4] = 1
m[5,6] = m[6,5] = 1
m[5,7] = m[7,5] = 1
m[5,8] = m[8,5] = 1
m[6,8] = m[8,6] = 1

g  = graph.adjacency(m, mode="undirected", weighted=T)
plot(g, edge.label=E(g)$weight)


# example 2: adjacency matrix of directed graph
g <- graph( c(1,2, 2,3, 3,4, 1,6, 6,5, 5,4, 4,1) )
E(g)$capacity <- c(3,1,2,10,1,3,2)
plot(g, edge.label=E(g)$capacity)
graph.maxflow(g, 1, 2)
graph.mincut(g, value.only=FALSE)
graph.mincut(g, 1, 2)


# example 3: directed graph (see example in lecture Beispiel with Ford-Fulkerson)
m = matrix(0, 8, 8)
m[1,2] = 9
m[1,4] = 9
m[2,4] = 9
m[2,5] = 5
m[3,4] = 6
m[4,6] = 3
m[4,8] = 8
m[5,6] = 8
m[5,7] = 5
m[5,8] = 1
m[6,8] = 4

g = graph.adjacency(m, mode="directed", weighted=T)
plot(g, edge.label=E(g)$weight)
E(g)$capacity = E(g)$weight
graph.maxflow(g, 1, 8)
E(g)

