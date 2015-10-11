neville <- function(x,xi,fi){
  
  n <- length(xi) - 1
  Q <- matrix(0, n+1, n+1)
  Q[,1] = fi
  
  for (i in 1:n) {
    for (j in 1:i){
      Q[i+1,j+1] <- ((x - xi[i-j +1])*Q[i+1,j] - (x - xi[i+1])*Q[i,j])/(xi[i+1] - xi[i-j+1])
    }
  } 
  return (Q)
}