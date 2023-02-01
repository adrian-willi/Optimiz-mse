genAlgDemo = function(nGeneration=100, nPopulation=100, sd_m=.1)
{
   population = array(1,c(2,nPopulation),dimnames=list(c("x","y"),paste("individual", 1:nPopulation, sep="")))
   fitness = rep(fitnessfunction(1,1),nPopulation)
   for (iter in 1:nGeneration)
   {
       print(iter)
       x = seq(0,1,.01)
       y = seq(0,1,.01)
       z = outer(x, y, fitnessfunction)
       contour(x,y,z,col = rainbow(12), main="exp(-(x+y))*(sin(x)*sin(y))^2",
          xlab="x",ylab="y",levels=c(0.01,0.1,.2,.3,.4,.5,.6,.7))
       points(population[1,],population[2,],pch=20,col="blue")
       Sys.sleep(.5)
       for(i in 1:nPopulation)
       {
           fitness[i] = fitnessfunction(population[1,i],population[2,i],population)
       }
       best       = order(fitness,decreasing = TRUE)
       population = population[,best]
       population_new = population
       if(iter > 3*nGeneration/4)
       {
           sd_m = sd_m*0.9
       }
       for(i in 1:nPopulation)# Elite Bypass
       {
           if(runif(1)<0.5)
           {
               index = selectParent(1,nPopulation)
               population_new[,i] = mutate(population[1,index],population[2,index],sd_m)
           }
           else
           {
               index = selectParent(2,nPopulation)
               i1 = index[1]
               i2 = index[2]
               population_new[,i] = crossover(population[1,i1],population[2,i1],population[1,i2],population[2,i2])
           }
       }
       population = population_new
   }
   best       = order(fitness,decreasing = TRUE)
   population = population[,best]
   points(population[1,1],population[2,1],col="red",pch=20)
   return(c(population[,1],fitnessfunction(population[1,1],population[2,1])))
}

fitnessfunction = function(x,y,population=NULL)
{
    omega = 4*pi
    sigma = 1
    ff = (sin(omega*x)*sin(omega*y))^2*exp(-(x+y)/sigma)
    if(x<0 || x>1 || y<0 || y>1)
    {
       ff = 0
    }
    if(is.null(population))
    {
       return(ff)
    }
    distance = sqrt( (x - sum(population[1,]))^2 + (y - sum(population[2,]))^2 )
    return(ff)
    #return(ff+distance)
}

mutate = function(x,y,sd_m)
{
    x = rnorm(1,x,sd_m)
    y = rnorm(1,y,sd_m)
    return(c(x,y))
}

crossover = function(x1,y1,x2,y2)
{
   if(fitnessfunction(x1,y2)>fitnessfunction(x2,y1))
   {
      return(c(x1,y2))
   }
   else
   {
      return(c(x2,y1))
   }
}

selectParent = function(n=1,m)
{
    xi = sort(sample(m,6))
    return(xi[1:n])
}

genAlgDemo(80,100,.05)
genAlgDemo(80,100,.08)






