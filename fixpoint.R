fixpoint <- function(g,p0,N,TOL){
  p <- numeric(N)
  for (i in 1:N){
    p[i] <- g(p0)
    if ( is.na(abs(p[i] - p0) < TOL) ) { break }
    p0 <- p[i]
  }
  return (p[1:N])
}