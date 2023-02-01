path = "~/Documents/EigeneDaten/15_HS2019/MSE_Optimization/R_Skripte_2019/"
source(paste(path,"vl08_gradientDescent.r",sep="")   , chdir = TRUE)
source(paste(path,"vl08_newton.r",sep="")           , chdir = TRUE)
source(paste(path,"vl09_newton_discretised.r",sep="") , chdir = TRUE)
source(paste(path,"vl09_broyden.r",sep="")          , chdir = TRUE)
source(paste(path,"vl09_broyden_discretised.r",sep=""), chdir = TRUE)
source(paste(path,"vl09_aitken.r",sep="")           , chdir = TRUE)

f   = function(x,y){return((x-2)^4+(x-2*y)^2)}
fx  = function(x,y){return(4*(x-2)^3+2*(x-2*y))}
fy  = function(x,y){return(-4*(x-2*y))}
fxx = function(x,y){return(12*(x-2)^2+2)}
fxy = function(x,y){return(-4)}
fyx = function(x,y){return(-4)}
fyy = function(x,y){return(8)}

animation = function(opt)
{
    x = seq(-0.2,4,.1)
    y = seq(-3,5,.1)
    z = outer(x, y, f)
    #image(x,y,z)
    #contour(x,y,z,col="pink",add=TRUE)
    #points(2,1,col="pink",pch=4)
    contour(x,y,z,col = rainbow(13), main="Bazarra-Shetty function",xlab="x",ylab="y")
	points(2,1, col="red", pch=4, cex=2 , lwd=2)
    points(opt[[1]][1],opt[[2]][1],pch=19,cex=1.5)
    readline(paste(0, round(opt[[1]][1],6), round(opt[[2]][1],6)))
	for(i in 1:(length(opt[[1]])-1))
	{
		lines (opt[[1]][(i):(i+1)],opt[[2]][(i):(i+1)])
		points(opt[[1]][i+1],opt[[2]][i+1],pch=19)
		readline(paste(i, round(opt[[1]][i+1],6), round(opt[[2]][i+1],6)))
	}
}

animation(gradientDescent(0,0,200,constStepSize))
animation(gradientDescent(1,0,200,constStepSize))
animation(gradientDescent(0,0,200,halving))
animation(gradientDescent(0,0,200,halvingWithParabolafitting))
animation(newton(0,0,30))
animation(newton_discretised(0,0,30,0.0001))
animation(broyden(0,0,50))
animation(broyden_discretised(0,0,50,0.001))
animation(aitken_(gradientDescent(0,0,200)))
animation(aitken_(broyden(0,0,50)))
animation(aitken_(newton(0,0,50)))
animation(aitken_(broyden_discretised(0,0,30,0.001)))

animation(newton_discretised_bad(100,100,40,0.001))
