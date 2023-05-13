source("rscript.r")

#make this accurate 

set.seed(5)
poisson_simulation <- function(lambda){
  sum_of_exponentials <- 0.0 
  counter <- 0 
  while (! (sum_of_exponentials > lambda )) {
    initial_random_number <- runif(1,0,1) 
    #length 1 lower bound 0 upper bound 1 
    simulated_exponential <- - log(1 - initial_random_number ) 
    #given by inversion method for parameter = 1 
    sum_of_exponentials <- sum_of_exponentials + simulated_exponential
    counter <- counter + 1 
  }
  #loop exit => sum_of_exponentials > lambda is true, and counter exponentials have been summed, return counter -1 
  return (counter -1 ) 
}

