hermit <- function(x,f,df){
n <- length(x) - 1
z <- matrix(0, n+1)
Q <- matrix(0, 2*(n+1), 2*(n+1))

for (i in 1:(n+1)){
  z[c(2*i)] <- x[i]
  z[c(2*i - 1)] <- x[i]
  for (j in 1:i){
    Q[2*j,1] <- f[j]
    Q[2*j-1,1] <- f[j]
    Q[2*j-1,2] <- df[j]
  }
  if (i != 1){
    Q[2*i,2] <- (Q[2*i-1,1] - Q[2*i-2,1])/(z[2*i-1] - z[2*i-2])
  }
}
for (m in 3:(2*n+2)){
  for (e in 3:m){
    Q[m,e] <- (Q[m,e-1] - Q[m-1,e-1])/(z[m] - z[m-e+1])
  }
}
return (Q)
}