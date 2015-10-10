Steffensen <- function(g, p0, epsilon, N) {
 # g is the function g(x)
 # p0 is the initial guess 
 # N is the max iteration
 # epsilon is sufficently small but greater than 0
  i <- 1
  while (i <= N){
    p1 <- g(p0) #compute p1 of i-1
    p2 <- g(p1) #compute p2 of i-1
    p <- p0 - (p1 - p0)^2 /(p2 - 2*p1 + p0) #compute p0 of i
    if (abs(p - p0) < epsilon) break 
    i <- i +1
    p0 = p #update p0
  }
  return (p)
}