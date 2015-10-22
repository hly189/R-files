bisection <- function(f,a,b,N, TOL){
  
  x <- numeric(N)
  for ( i in 1:N){
    p <- (a+b)/2
    x[i] <- p
    if ( p-a < TOL) { break }
    if (f(a) * f(p) > 0) {
      a = p
    } else {
      b = p
    }
  }
  return (x[1:N])
}