Horner <- function(a,x0) {
  # a is the coefficients a0,...,a1
  # x0 is initial guess 
  n <- length(a) - 1
  y <- a[1] #Compute b_n for P
  z <- a[1] #Compute b_n-1 for Q
  for (j in 1:n){ 
    y <- x0*y + a[j]
    z <- x0*z + y
  }
  y = x0*y + a[n+1] #Compute b0 for P
  output <- list(y,z)
  return (output)
}