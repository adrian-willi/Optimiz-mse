floydWarshall = function(m)
{
	n = dim(m)[1]
	print(m)
	#m_old=m
	for(k in 1:n)
	{
		for(i in 1:n)
		{
				for(j in 1:n)
				{
                    m[i,j] = min(m[i,j],m[i,k]+m[k,j])
				}
		}
		print(m)
		#print(m_old-m)
		#m_old = m
	}
}

# example undirected graph
m = matrix(Inf, 6, 6)
diag(m) = 0
m[1,2] = m[2,1] =  60
m[1,3] = m[3,1] = 160
m[1,5] = m[5,1] = 700
m[1,6] = m[6,1] = 600
m[2,3] = m[3,2] = 220
m[2,4] = m[4,2] = 140
m[2,6] = m[6,2] = 800
m[3,4] = m[4,3] =  20
m[3,5] = m[5,3] = 900
m[3,6] = m[6,3] = 500
m[4,5] = m[5,4] = 800
m[4,6] = m[6,4] = 300
m[5,6] = m[6,5] =  40
floydWarshall(m)

# example undirected graph
m = matrix(Inf, 5, 5)
diag(m) = 0
m[1,2] = m[2,1] =  20
m[1,3] = m[3,1] =  40
m[2,3] = m[3,2] =  80
m[2,4] = m[4,2] = 300
m[2,5] = m[5,2] = 400
m[3,4] = m[4,3] = 200
m[4,5] = m[5,4] =  60
floydWarshall(m)



# example directed graph
m = matrix(Inf, 4, 4)
diag(m) = 0
m[1,2] = 10
m[1,3] = 60
m[2,3] = 20
m[2,4] = 30
m[3,4] = 20
m[4,1] = 10
floydWarshall(m)


library(igraph)
m_=m
m_[m_>1000]=0
g  = graph.adjacency(m_, mode="directed", weighted=T)
plot(g, edge.label=E(g)$weight)

