# Define the two-dimensional function f with first and second partial derivatives:
# f
# fx
# fy
# fxx
# fxy
# fyx
# fyy

# x_0 is the starting value for the x-coordinate
# y_0 is the starting value for the y-coordinate
# n is the number of iterations
newton = function(x_0, y_0, n=10)
{
  x = c(x_0)
  y = c(y_0)
  for(i in 1:(n-1))
  {
    point_old   = matrix(c(x[1],y[1]),2)
    HesseMatrix = matrix(c(fxx(x[1],y[1]),fyx(x[1],y[1]),fxy(x[1],y[1]),fyy(x[1],y[1])),2)
    if(abs(det(HesseMatrix))<1e-12) break;
    gradient    = matrix(c(fx(x[1],y[1]),fy(x[1],y[1])),2)
    point_new   = point_old - (solve(HesseMatrix)%*%gradient)
    x = c(point_new[1], x)
    y = c(point_new[2], y)
  }
  return( list(rev(x), rev(y)) )
}
