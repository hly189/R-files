newton <- function (f, df, x0, N, epsilon) {
  # f is the function and df is its derivative
  # x0 is intial guess 
  # epsilon is somenumber which is really small but greater than 0
   #N is the number of iteration
  for (i in 1:N) {
    newton <- x0 - f(x0)/df(x0)
    if(abs(newton - x0) < epsilon) break 
    x0 <- newton
    cat(sprintf("\"%f\" \"%f\"\n", df$i, df$newton))
    }
  return (newton)
}