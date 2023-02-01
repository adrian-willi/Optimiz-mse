path = "~/Documents/EigeneDaten/15_HS2019/MSE_Optimization/R_Skripte_2019/"
source(paste(path,"vl12_hillClimbing.r",sep=""), chdir = TRUE)
source(paste(path,"vl12_simulatedAnnealing.r",sep=""), chdir = TRUE)
source(paste(path,"vl12_tabuSearch.r",sep=""), chdir = TRUE)


g = function(x,y){return((1-x)^2 + 100*(y-x^2)^2 )}
minimum = c(1,1)

x = seq(-1.5,2,.01)
y = seq(-0.5,3,.01)
z = outer(x, y, g)

levels=c(1,10,50, 100,200,400,600,800)
col   = topo.colors(length(levels))
contour(x,y,z,levels=levels,col=col, main="Zielfunktion",xlab="x",ylab="y")

Animation = function(opt)
{
	contour(x,y,z,levels=levels,col=col, main="Zielfunktion",xlab="x",ylab="y")
	points(minimum[1],minimum[2], col="red", pch=4, cex=2 , lwd=2)
    points(opt[[1]][1],opt[[2]][1],pch=19,cex=1.5)
    readline(paste(0, round(opt[[1]][1],6), round(opt[[2]][1],6)))
	for(i in 1:(length(opt[[1]])-1))
	{
		lines (opt[[1]][(i):(i+1)],opt[[2]][(i):(i+1)])
		points(opt[[1]][i+1],opt[[2]][i+1],pch=19)
		readline(paste(i, round(opt[[1]][i+1],6), round(opt[[2]][i+1],6)))
	}
}

Animation(hillClimbing(0,0,1000,g,.1))
Animation(hillClimbing(0,0,10000,g,.5))
Animation(hillClimbing(0,0,10000,g,.01))

Animation(tabuSearch(0,0,1000,g))

Animation(simulatedAnnealing(0,0,1000,g,0.1,0))
Animation(simulatedAnnealing(0,0,1000,g,0.1,0.1))
Animation(simulatedAnnealing(0,0,1000,g,0.1,0.2))
Animation(simulatedAnnealing(0,0,1000,g,0.1,0.5))

Animation(simulatedAnnealing(0,0,1000,g,0.5,1))
Animation(simulatedAnnealing(0,0,1000,g,0.5,10))

Animation(simulatedAnnealing(0,0,1000,g,1,100))




g = function(x,y){return( -cos(x)^2 * cos(y)^2 + (y^2 + x^2)/10 )}
minimum = c(0,0)

x = seq(-10,10,.1)
y = seq(-10,10,.1)
z = outer(x, y, g)

levels=c(0,0.1,0.2,0.3,0.5,0.6,0.7,0.9,1,2,3,5,10)
col   = topo.colors(length(levels))
contour(x,y,z,levels=levels,col=col, main="Zielfunktion",xlab="x",ylab="y")

Animation(hillClimbing(10,10,1000,g))
Animation(hillClimbing(10,10,1000,g,0.5))
Animation(tabuSearch(10,10,1000,g,.1))
Animation(tabuSearch(10,10,1000,g,1))
Animation(simulatedAnnealing(10,10,1000,g,0.1))
Animation(simulatedAnnealing(10,10,1000,g,0.5))
Animation(simulatedAnnealing(10,10,1000,g,0.2,.3))
Animation(simulatedAnnealing(10,10,1000,g,0.5,.5))
Animation(simulatedAnnealing(10,10,1000,g,0.5,1))
Animation(simulatedAnnealing(10,10,1000,g,0.5,5))
Animation(simulatedAnnealing(10,10,1000,g,1,10))
Animation(simulatedAnnealing(10,10,1000,g,2,10))
Animation(simulatedAnnealing(10,10,1000,g,10,10))


Animation(simulatedAnnealing(2,2,1000,g,.1,.1))

