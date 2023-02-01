plotTsp = function(d)
{
    plot(d[1,],d[2,],pch=19, main="Demo: TSP mit genetischen Algorithmen",xlim=c(0,1.05),col="red")
    text(d[1,],d[2,],dimnames(d)[[2]],pos=4)
}

plotPath = function(tsp,d,col="black")
{
    tsp = c(tsp,tsp[1])
    lines(d[1,tsp],d[2,tsp],col=col)
}

fitness = function(tsp,d)
{
    score = 0
    tsp   = c(tsp,tsp[1])
    for(i in 2:length(tsp))
    {
        score = score + sqrt( (d[1,tsp[i]] - d[1,tsp[i-1]])^2 + (d[2,tsp[i]] - d[2,tsp[i-1]])^2 )
    }
    return(-score)
}

mutate = function(tsp,d)
{
    l = length(tsp)
    x = sample(l-1,1)
    tsp_ = c(tsp[l:(x+1)],tsp[1:x])
    return(tsp_)
}

mutate2 = function(tsp,d)
{
    l = length(tsp)
    x = sort(sample(2:(l-1), 2))
    y = x[2]
    x = x[1]
    # Inversion
    tsp_ = c(tsp[1:(x-1)],rev(tsp[x:y]),tsp[(y+1):l])
    # Positionsmutation
    #if(x+1 != y)
    #{
    #    tsp_ = c(tsp[1:(x-1)],tsp[y],tsp[(x+1):(y-1)],tsp[x],tsp[(y+1):l])
    #}
    #else
    #{
    #    tsp_ = c(tsp[1:(x-1)],tsp[y],tsp[x],tsp[(y+1):l])
    #}
    return(tsp_)
}

crossover = function(tsp1, tsp2, d)
{
    #tsp1 = sample(8)
    #tsp2 = sample(8)
    #print(tsp1)
    #print(tsp2)
    Nachbarn = list()
    n = length(tsp1)
    for(i in 1:n)
    {
        nachbarn = c()
        ind = which(tsp1 == i)
        if(ind == 1)
        {
            nachbarn = c(nachbarn, tsp1[2], tsp1[n])
        }
        else if(ind == n)
        {
            nachbarn = c(nachbarn, tsp1[n-1], tsp1[1])
        }
        else
        {
            nachbarn = c(nachbarn, tsp1[ind-1], tsp1[ind+1])
        }
        ind = which(tsp2 == i)
        if(ind == 1)
        {
            nachbarn = c(nachbarn, tsp2[2], tsp2[n])
        }
        else if(ind == n)
        {
            nachbarn = c(nachbarn, tsp2[n-1], tsp2[1])
        }
        else
        {
            nachbarn = c(nachbarn, tsp2[ind-1], tsp2[ind+1])
        }
        nachbarn = union(nachbarn,nachbarn)
        Nachbarn[[i]] = nachbarn
    }
    #return(Nachbarn)
    child = c()
    i = sample(n,1)
    for(nnn in 1:n)
    {
        child = c(child,i)
        #print(paste("Lösche ",i))
        #print(Nachbarn)
        for(e in 1:n)
        {
            Nachbarn[[e]] = setdiff(Nachbarn[[e]],i)
        }
        i_neu = NA
        if(length(Nachbarn[[i]])>1)
        {
            i_neu = sample(Nachbarn[[i]],1)
        }
        if(length(Nachbarn[[i]])==1)
        {
            i_neu = Nachbarn[[i]]
        }
        if(length(Nachbarn[[i]])==0)
        {
            elem = union(unlist(Nachbarn),unlist(Nachbarn))
            if(length(elem)==1)
            {
                i_neu = elem
            }
            else if(length(elem)>1)
            {
                i_neu = sample(union(unlist(Nachbarn),unlist(Nachbarn)),1)
            }
        }
        i = i_neu
        #print(paste("Wähle ",i))
    }
    #print(Nachbarn)
    return(child)
}

crossover2 = function(tsp1,tsp2,d)
{
    return(tsp1)
    
    l = length(tsp1)
    x = sample(l-1,1)
    child1_ = c(tsp1[1:x],tsp1[(x+1):l])
    child2_ = c(tsp2[1:x],tsp2[(x+1):l])
    if(fitness(child1_,d) > fitness(child2_,d))
    {
        return(child1_)
    }
    else
    {
        return(child2_)
    }
}

tspDemo = function(nGeneration=50, nPopulation=10, n=30, seed=2)
{
    # Initialisiere zufällige Städe
    set.seed(seed)
    n = n
    d = array(NA,c(2,n),dimnames=list(c("x","y"),paste("s", 1:n, sep="")))
    d[1,] = sort(runif(n))
    d[2,] = (runif(n))
    # Initialisiere genetischen Algorithmus
    nPopulation = nPopulation
    population  = array(NA,c(nPopulation,n))
    fitness     = rep(NA, nPopulation)
    for(i in 1:nPopulation)
    {
        population[i,] = sample(n)
        fitness[i]     = fitness(population[i,],d)
    }
    plotTsp(d)
    Sys.sleep(1)
    for(generation in 1:nGeneration)
    {
        Sys.sleep(.1)
        population_     = array(NA,c(10*nPopulation,n))
        fitness_        = rep(NA,10*nPopulation)
        best            = order(fitness)[nPopulation]
        population_[1,] = population[best,]
        fitness_[1]     = fitness(population_[1,],d)
        for(i in 2:(8*nPopulation))
        {
            population_[i,] = mutate(population[sample(nPopulation,1),],d)
            fitness_[i]     = fitness(population_[i,],d)
        }
        for(i in (9*nPopulation+1):(10*nPopulation))
        {
            population_[i,] = crossover(population[sample(nPopulation,1),],population[sample(nPopulation,1),],d)
            fitness_[i]     = fitness(population_[i,],d)
        }
        population = population_[order(fitness_,decreasing=T)[1:nPopulation],]
        fitness    = fitness_   [order(fitness_,decreasing=T)][1:nPopulation]
        plotTsp(d)
        title(paste("Generation: ", generation, ", Score: ", round(fitness[1],2)) ,line=0.5)
        plotPath(population[1,],d)
    }
}

#tspDemo()
