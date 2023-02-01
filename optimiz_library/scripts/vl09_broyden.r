# Quasi-Newton method: Broyden
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
broyden = function(x_0, y_0, n=10)
{
  x = c(x_0)
  y = c(y_0)
  # Calculate the Hessian and its inverse only in the first iteration
  fx_ = fx(x[1],y[1])
  fy_ = fy(x[1],y[1])
  fxx_= fxx(x[1],y[1])
  fxy_= fxy(x[1],y[1])
  fyx_= fyx(x[1],y[1])
  fyy_= fyy(x[1],y[1])
  HesseMatrix = matrix(c(fxx_,fyx_,fxy_,fyy_),2)
  HesseMatrixInvers = solve(HesseMatrix)
  gradient    = matrix(c(fx_,fy_),2)
  point_old   = matrix(c(x[1],y[1]),2)
  point_new   = point_old - ((solve(HesseMatrix))%*%gradient)
  x = c(point_new[1,1], x)
  y = c(point_new[2,1], y)
  for(i in 1:(n-2))
  {
    s   =  point_new - point_old
    point_old = point_new
    fx_ = fx(x[1],y[1])
    fy_ = fy(x[1],y[1])
    d   =  matrix(c(fx_,fy_),2) - gradient
    gradient  = matrix(c(fx_,fy_),2)
    HesseMatrixInvers = (HesseMatrixInvers - ((HesseMatrixInvers%*%d-s)%*%t(s)%*%HesseMatrixInvers)/(c(t(s)%*%HesseMatrixInvers%*%d)))
    point_new    = point_old - (HesseMatrixInvers%*%gradient)
    x = c(point_new[1,1], x)
    y = c(point_new[2,1], y)
  }
  return(list(rev(x),rev(y)))
}
