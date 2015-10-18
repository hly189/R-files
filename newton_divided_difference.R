newton_divided_difference <- function(x,f){
  n <- length(x) - 1
  F <- matrix(0, n+1, n+1)
  F[,1] = f
  
  for (i in 1:n) {
    for (j in 1:i) {
      F[i+1,j+1] = (F[i+1,j] - F[i,j])/(x[i+1] - x[i-j+1])
    }
  }
  return (F)
}