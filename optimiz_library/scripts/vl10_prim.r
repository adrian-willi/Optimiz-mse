prim = function(m, node=1)
{
   V = c(node)
   E = list()
   S_Edges   = list()
   S_Weights = c()
   while(length(V) < dim(m)[1])
   {
     # reachable nodes
     nodes   = which(m[,node]<Inf)
     # weights of nodes
     weights = m[,node][m[,node]<Inf]
     for(k in 1:length(weights))
     {
       S_Edges   = c(S_Edges  , list(c(node,nodes[k])))
       S_Weights = c(S_Weights, weights[k])
     }
     # search reachable node with lowest weight
     while(TRUE)
     {
       index     = which.min(S_Weights)
       kante     = S_Edges[[index]]
       S_Edges   = S_Edges[-index]
       S_Weights = S_Weights[-index]
       if(xor(is.element(kante[1],V),is.element(kante[2],V)))
       {
           node = kante[1]
           if(is.element(kante[1],V))
           {
              node = kante[2]
           }
           V = c(V,node)
           E = c(E, list(kante))
           break;
       }
     }
   }
   return(list(V,E))
}

# Beispiel:
m = matrix(Inf, 6, 6)
m[1,2] = m[2,1] = 2
m[1,4] = m[4,1] = 1
m[2,3] = m[3,2] = 7
m[2,4] = m[4,2] = 3
m[2,5] = m[5,2] = 5
m[3,6] = m[6,3] = 6
m[4,5] = m[5,4] = 4
m[5,6] = m[6,5] = 8

prim(m)


m_ = m
m_[m_>=Inf]=0
g  = graph.adjacency(m_, mode="undirected", weighted=T)
plot(g, edge.label=E(g)$weight)
