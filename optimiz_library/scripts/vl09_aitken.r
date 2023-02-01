# speed up convergence with aitken
aitken = function(x)
{
    l = length(x)
    return(x[1:(l-2)]-(x[2:(l-1)]-x[1:(l-2)])^2/(x[3:(l)]-2*x[2:(l-1)]+x[1:(l-2)]))
}

aitken_ = function(sequenz2dim)
{
  x = aitken(sequenz2dim[[1]])
  y = aitken(sequenz2dim[[2]])
  return(list(x,y))
}
