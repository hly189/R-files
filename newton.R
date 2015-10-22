newton <- function (f, df, x0, N, epsilon) {
  # f is the function and df is its derivative
  # x0 is intial guess 
  # epsilon is some numbers which is really small but greater than 0
   #N is the number of iteration
  p <- numeric(N)
  for (i in 1:N) {
    newton <- x0 - f(x0)/df(x0) #compute newton's method 
    p[i] <- newton
    if(abs(newton - x0) < epsilon) break 
    x0 <- newton #update x0
    }
  return (p[1:N])
}