g = function(x,y){return((1-x)^2 + 100*(y-x^2)^2 )}

x = seq(-1.5,2,.01)
y = seq(-0.5,3,.01)
z = outer(x, y, g)

levels=c(10,50, 100,200,400,600,800)
col   = topo.colors(length(levels))
contour(x,y, z,levels=levels,col=col)
points(1,1,pch=19, col="red")


image(x,y, z)
contour(x,y, z,levels=levels,col="pink",add=T)
points(1,1,col="pink",pch=4)



tabuSearch = function(x,y,n=100,f=function(x,y){return((1-x)^2 + 100*(y-x^2)^2 )},sd=0.1)
{
  x_   = c(x)
  y_   = c(y)
  tabu = pi
  for(i in 1:(n-1))
  {
    phi  = runif(1,0,2*pi)
    
    r    = abs(rnorm(1,0,sd))
    if(min(abs(phi - tabu),2*pi-abs(phi - tabu))<pi/2)
    {
      phi = (phi + pi)%%(2*pi)
    }
    #print(tabu)
    #print(phi)
    tabu = (phi + pi)%%(2*pi)
    mini = Inf
    index = -100
    for(i in -60:60)
    {    
      xSchritt = r*cos(phi+i/360*(2*pi))
      ySchritt = r*sin(phi+i/360*(2*pi))
      mini_ = f(x+xSchritt,y+ySchritt)
      if(mini_<mini)
      {
        mini  = mini_
        index = i
      }
    }
    if(f(x+r*cos(phi+index/360*(2*pi)),y+r*sin(phi+index/360*(2*pi))) < f(x,y))
    {
	    x = x+r*cos(phi+index/360*(2*pi))
	    y = y+r*sin(phi+index/360*(2*pi))
	    x_ = c(x,x_)
	    y_ = c(y,y_)
    }
    
    #print(index)
    #print("__")
  }
  return(list(rev(x_),rev(y_)))
}
ts = tabuSuche(0,0,100)

# Lecture
#pdf("TabuSuche.pdf",10.8/1.4,7.5/1.4)
#contour(x,y, z,levels=levels,col=col,main="Tabu Suche (100Iterationen, r, alpha) Gegenrichtung Tabu")
#points(1,1,pch=19, col="red")
#lines(ts[[1]],ts[[2]])
#points(ts[[1]],ts[[2]],pch=19)
#dev.off()


