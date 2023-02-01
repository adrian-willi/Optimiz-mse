# depth and breadth search

m = matrix(Inf, 6, 6)
m[1,2] = m[2,1] = 2
m[1,4] = m[4,1] = 1
m[2,3] = m[3,2] = 7
m[2,4] = m[4,2] = 3
m[2,5] = m[5,2] = 5
m[3,6] = m[6,3] = 6
m[4,5] = m[5,4] = 4
m[5,6] = m[6,5] = 8

m = matrix(Inf, 8, 8)
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

breadthsearch = function(m, node=1, target=NULL)
{
    diag(m) = Inf
    fifo    = c(node)
    while(length(fifo)>0)
    {
        node  = fifo[1]
        print(node)
        fifo = fifo[-1]
        visited = which(m[node,]<Inf)
        fifo = c(fifo, visited)
        m[,node   ] = Inf
        m[,visited] = Inf
    }
}
breadthsearch(m)

depthsearch = function(m, node=1, target=NULL)
{
    diag(m) = Inf
    stack   = c(node)
    while(length(stack)>0)
    {
        while(length(which(m[node,]<Inf))>0)
        {
            node_ = which(m[node,]<Inf)[1]
            stack = c(node_, stack)
            m[,node] = Inf
            node = node_
        }
        node  = stack[1]
        stack = stack[-1]
        m[,node] = Inf
        print(node)
        node  = stack[1]
    }
}
depthsearch(m)



