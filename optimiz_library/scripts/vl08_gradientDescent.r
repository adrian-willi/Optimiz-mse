# Define the twodimensional function f and its partial derivatives:
# f
# fx
# fy

# x_0 is the starting value for the x-coordinate
# y_0 is the starting value for the y-coordinate
# n is the number of iterations
gradientDescent = function(x_0, y_0, n=10, stepwidth=halvingWithParabolafitting)
{
  x = c(x_0)
  y = c(y_0)
  for(i in 1:(n-1))
  {
    # gradient information
    rx = fx(x[1], y[1])
    ry = fy(x[1], y[1])
    # define the stepwidth beta
    beta = stepwidth(f, x[1], y[1], rx, ry)
    x = c(x[1]-beta*rx, x)
    y = c(y[1]-beta*ry, y)
  }
  return( list(rev(x), rev(y)) )
}

#successive halving
halving = function(f, x, y, rx, ry)
{
  beta    = 1
  minimum = f(x,y)
  if( f(x-beta*rx,y-beta*ry) >= minimum )
  {
    repeat
    {
      minimum_ = f(x-beta*rx,y-beta*ry)
      if(minimum_ < minimum){break}
      beta = beta/2
    }
  }
  else
  {
    while(f(x-beta*rx,y-beta*ry) > f(x-2*beta*rx,y-2*beta*ry))
    {
        beta = 2*beta
    }
  }
  return(beta)
}

#successive halving with parabola fitting
halvingWithParabolafitting = function(f, x,y, rx,ry)
{
  beta = halving(f,x,y,rx,ry)
  # fit parabola:
  beta_1 = 0
  beta_2 = beta
  beta_3 = 2*beta
  f_beta_1 = f(x-beta_1*rx,y-beta_1*ry)
  f_beta_2 = f(x-beta_2*rx,y-beta_2*ry)
  f_beta_3 = f(x-beta_3*rx,y-beta_3*ry)
  # Find minimum in [0,2*beta] = [beta_1,beta_3]
  # P(beta) = a*beta^2 + b*beta + c
  # P'(beta) = 2a*beta + b = 0 <=> beta = -b/(2a)
  l = solve(matrix(c(beta_2^2,beta_3^2,beta_2,beta_3),2)) %*% c(f_beta_2-f_beta_1,f_beta_3-f_beta_1)
  beta_star = -l[2]/(2*l[1])
  # alternative calculation:
  beta_star = beta/2 * (3*f_beta_1 - 4*f_beta_2 + 1*f_beta_3)/(1*f_beta_1 - 2*f_beta_2 + 1*f_beta_3)
  # improvement?
  if( f(x-beta_star*rx,y-beta_star*ry) < f(x-beta*rx,y-beta*ry) )
  {
    beta = beta_star
  }
  return(beta)
}

# constant step size
constStepSize = function(f, x, y, rx, ry)
{
    beta    = 0.1
    return(beta)
}
