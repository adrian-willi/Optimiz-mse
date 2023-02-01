# Ford Fulkerson
m = matrix(0, 6, 6)
m[1,2]  = 2
m[1,4]  = 1
m[2,3]  = 7
m[2,4]  = 3
m[2,5]  = 5
m[3,6]  = 6
m[4,5]  = 4
m[5,6]  = 8

# minimal example script:
m = matrix(0, 6, 6)
m[1,2] = 1
m[1,3] = 1
m[2,4] = 1
m[3,4] = 1
m[3,5] = 1
m[4,6] = 1
m[5,6] = 1
# minimal example script 2:
m = matrix(0, 6, 6)
m[1,2] = 1
m[2,3] = 1
m[3,6] = 1
m[1,4] = 1
m[4,3] = 1
m[2,5] = 1
m[5,6] = 1

depthsearch_ff = function(m, node, target)
{
    start       = node
    diag(m)     = 0
    stack       = c(node)
    output      = c()
    predecessor = c()
    while(length(stack)>0)
    {
        while(length(which(m[node,]>0))>0)
        {
            node_ = which(m[node,]>0)[1]
            stack = c(node_, stack)
            m[,node] = 0
            node = node_
        }
        node  = stack[1]
        stack = stack[-1]
        m[,node] = 0
        output  = c(output, node)
        node  = stack[1]
        predecessor = c(predecessor, node)
    }
    if(is.element(target,output) )
    {
        path = c(target)
        node = target
        while(node != start)
        {
            node = predecessor[which(output==node)]
            path = c(path,node)
        }
        return(rev(path))
    }
    return(c())
}

fordfulkerson = function(m, start, target)
{
    m_rest = m
    while(TRUE)
    {
        ds = depthsearch_ff(m_rest, start, target)
        if(is.element(target,ds))
        {
            cap = Inf
            for(i in 1:(length(ds)-1) )
            {
                cap = min(cap,m_rest[ds[i],ds[i+1]])
            }
            for(i in 1:(length(ds)-1) )
            {
                m_rest[ds[i],ds[i+1]] = m_rest[ds[i],ds[i+1]] - cap
                m_rest[ds[i+1],ds[i]] = m_rest[ds[i+1],ds[i]] + cap
            }
            print(cap)
            print(ds)
        }
        else
        {
            break;
        }
    }
}

fordfulkerson(m,1,6)
