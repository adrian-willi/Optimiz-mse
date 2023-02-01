g = function(x,y){return((1-x)^2 + 100*(y-x^2)^2 )}

x = seq(-1.5,2,.01)
y = seq(-0.5,3,.01)
z = outer(x, y, g)

levels=c(10,50, 100,200,400,600,800)
col   = topo.colors(length(levels))
contour(x,y, z,levels=levels,col=col)
points(1,1,pch=19, col="red")


simulatedAnnealing = function(x,y,n=100,f=function(x,y){return((1-x)^2 + 100*(y-x^2)^2 )},sd=0.1,T=NULL)
{
  x_ = c(x)
  y_ = c(y)
  for(i in 1:(n-1))
  {
      x_vorschlag = rnorm(1, x, sd)
      y_vorschlag = rnorm(1, y, sd)
    
    score_alt = f(x,y)
    score_neu = f(x_vorschlag, y_vorschlag)
    if(is.null(T))
    {
        T =  ((n-i)/n)/100
    }
    #konstant T=1/100
    if(exp(-(score_neu-score_alt)/T)>runif(1,0,1))
    {
      x = x_vorschlag
      y = y_vorschlag
      x_ = c(x,x_)
      y_ = c(y,y_)
    }

  }
  return(list(rev(x_),rev(y_)))
}
sa = simulatedAnnealing(0,0,100)

# Lecture
#pdf("SimulatedAnnealing.pdf",10.8/1.4,7.5/1.4)
#contour(x,y, z,levels=levels,col=col,main="Simulated Annealing (100 Iterationen, sigma=0.1)")
#points(1,1,pch=19, col="red")
#lines(sa[[1]],sa[[2]])
#points(sa[[1]],sa[[2]],pch=19)
##lines(sa[[1]],sa[[2]],col=gray(0.5))
##points(sa[[1]],sa[[2]],col=gray((length(sa[[1]]):1)/(1.2*length(sa[[1]]))),pch=19)
#dev.off()
