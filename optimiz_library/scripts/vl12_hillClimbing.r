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



hillClimbing = function(x,y,n=100,f=function(x,y){return((1-x)^2 + 100*(y-x^2)^2 )},sd=0.1)
{
  x_ = c(x)
  y_ = c(y)
  for(i in 1:(n-1))
  {
    x_vorschlag = rnorm(1, x, sd)
    y_vorschlag = rnorm(1, y, sd)
    if(f(x_vorschlag,y_vorschlag)<f(x,y))
    {
      x = x_vorschlag
      y = y_vorschlag
      x_ = c(x,x_)
      y_ = c(y,y_)
    }
  }
  return(list(rev(x_),rev(y_)))
}
hc = hillClimbing(0,0)

# Lecture
#pdf("HillClimbing.pdf",10.8/1.4,7.5/1.4)
#contour(x,y, z,levels=levels,col=col,main="Randormisiertes Hill Climbing (100 Iterationen, sigma=0.1)")
#points(1,1,pch=19, col="red")
#lines(hc[[1]],hc[[2]])
#points(hc[[1]],hc[[2]],pch=19)
#dev.off()
