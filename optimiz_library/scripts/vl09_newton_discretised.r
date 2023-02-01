# Define the two-dimensional function f without first and second partial derivatives:
# f

# x_0 is the starting value for the x-coordinate
# y_0 is the starting value for the y-coordinate
# n is the number of iterations
# epsilon controls the approximation of the derivative
newton_discretised = function(x_0, y_0, n=10, epsilon=0.001)
{
    x = c(x_0)
    y = c(y_0)
    for(i in 1:(n-1))
    {
        point_old = matrix(c(x[1],y[1]),2)
        fx_ = (f(x[1]+epsilon/2,y[1]) - f(x[1]-epsilon/2,y[1]))/epsilon
        fy_ = (f(x[1],y[1]+epsilon/2) - f(x[1],y[1]-epsilon/2))/epsilon
        fxx_= (f(x[1]+epsilon/2,y[1]) - 2*f(x[1],y[1]) + f(x[1]-epsilon/2,y[1]) )/(epsilon/2)^2
        fxy_= (f(x[1]+epsilon/2,y[1]+epsilon/2) - f(x[1]-epsilon/2,y[1]+epsilon/2)  - f(x[1]+epsilon/2,y[1]-epsilon/2) + f(x[1]-epsilon/2,y[1]-epsilon/2) )/(epsilon^2)
        fyx_= fxy_
        fyy_= (f(x[1],y[1]+epsilon/2) - 2*f(x[1],y[1]) + f(x[1],y[1]-epsilon/2) )/(epsilon/2)^2
        HesseMatrix = matrix(c(fxx_,fyx_,fxy_,fyy_),2)
        gradient    = matrix(c(fx_,fy_),2)
        point_new    = point_old - ((solve(HesseMatrix))%*%gradient)
        x = c(point_new[1,1], x)
        y = c(point_new[2,1], y)
    }
    return(list(rev(x),rev(y)))
}


newton_discretised_bad = function(x_0, y_0, n=10, epsilon=0.001)
{
    x = c(x_0)
    y = c(y_0)
    for(i in 1:(n-1))
    {
        point_old = matrix(c(x[1],y[1]),2)
        fx_ = (f(x[1]+epsilon,y[1]) - f(x[1],y[1]))/epsilon
        fy_ = (f(x[1],y[1]+epsilon) - f(x[1],y[1]))/epsilon
        fxx_= (f(x[1]+epsilon,y[1]) - 2*f(x[1],y[1]) + f(x[1]-epsilon,y[1]) )/epsilon^2
        fxy_= (f(x[1]+epsilon,y[1]+epsilon) - f(x[1]-epsilon,y[1]+epsilon)  - f(x[1]+epsilon,y[1]-epsilon) + f(x[1]-epsilon,y[1]-epsilon) )/(4*epsilon^2)
        fyx_= fxy_
        fyy_= (f(x[1],y[1]+epsilon) - 2*f(x[1],y[1]) + f(x[1],y[1]-epsilon) )/epsilon^2
        HesseMatrix = matrix(c(fxx_,fyx_,fxy_,fyy_),2)
        gradient    = matrix(c(fx_,fy_),2)
        point_new    = point_old - ((solve(HesseMatrix))%*%gradient)
        x = c(point_new[1,1], x)
        y = c(point_new[2,1], y)
    }
    return(list(rev(x),rev(y)))
}
