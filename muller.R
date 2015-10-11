Muller <- function(f,p0, p1, p2, TOL, N){
  
  h1 <- p1 - p0
  h2 <- p2 - p1
  delta1 <- (f(p1) - f(p0))/h1
  delta2 <- (f(p2) - f(p1))/h2
  d = (delta2 - delta1)/(h2 + h1)
  for (i in 3:N) {
    b = delta2 + h2*d
    c = complex(sqrt(b^2 - 4*f(p2)*d))
    if ( abs(b-c) < abs(b+c) ) {
      E <- b + c
    } else {
      E <- b - c
    }
    h <- -2*f(p2)/E
    p <- p2 + h
    if (abs(h) < TOL ) break 
    p0 = p1
    p1 = p2
    h1 = p1 - p0
    h2 = p2 - p1
    delta1 <- (f(p1) - f(p0))/h1
    delta2 <- (f(p2) - f(p1))/h2
    d = (delta2 - delta1)/(h2 + h1)
  }
  return (p)
}