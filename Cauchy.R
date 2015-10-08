#This function is used to create Cauchy Matrix 
Cauchy <- function(x,y){
  Cauchy <- matrix(0, length(x), length(x))
  for (i in 1:length(x)){
    for (j in 1:length(y)){
      Cauchy[i,j] <- 1/(x[i] + y[j])
    }
  }
}