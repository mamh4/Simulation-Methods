source("rscript.r")

#make this accurate 

set.seed(5)
poisson_simulation <- function(lambda, size){
  simulated_poisson <- rep(0, size)
  for (i in 1:size){
    sum_of_exponentials <- 0.0 
    counter <- 0 
    while (! (sum_of_exponentials > lambda )) {
      initial_random_number <- runif(1,0,1) 
      #length 1 lower bound 0 upper bound 1 
      simulated_exponential <- -log(1 - initial_random_number ) 
      #given by inversion method for parameter = 1 
      sum_of_exponentials <- sum_of_exponentials + simulated_exponential
      counter <- counter + 1 
    }
    simulated_poisson[i] <- counter -1
  }
  return (simulated_poisson)
}

exponential_simulation <- function(lambda, size){
  simulated_exponential <- rep(0, size)
  for (i in 1:size){
    initial_random_number <- runif(1,0,1)
    single_simulated_exponential <- - log(1 - initial_random_number )/lambda
    #given by inversion method
    simulated_exponential[i] <- single_simulated_exponential
  }
  return (simulated_exponential)
}

sample_mean <- function(random_vector){
  sum <- 0 
  for (i in 1:length(random_vector)){
    sum <- sum + random_vector[i]
  }
  if (length(random_vector) == 0 ){
    return (0)
  } 
  else {
    return (sum/length(random_vector)) 
  }
}

sample_variance <- function(random_vector){
  sum <- 0 
  sample_mean <- sample_mean(random_vector)
  for (i in 1:length(random_vector)){
    sum <- sum + (random_vector[i] - sample_mean)^2
  }
  if (length(random_vector)>1){
    return (sum / (length(random_vector) - 1) ) 
  }
  else {
    return (0) 
  }
}

theoretical_poisson <- function(lambda, k){
  return (exp(-lambda ) * lambda^k / factorial(k )  ) 
}
#gen for 0 to 7 with our lambda then take 1- sum of previous for final category (0 observations, values of 8 and above ) 

#negative binomial with parameters r p 
# p estimator -> var / e 
# r estimator -> e * p / (1 - p ) 

negativeBinomialPEstimator <- function(random_vector){
  return(sample_variance(random_vector ) / sample_mean(random_vector )) 
}

negativeBinomialREstimator <- function(random_vector){
  p <- negativeBinomialPEstimator(random_vector)
  return(sample_mean(random_vector )*p/(1-p)) 
}

#compute mean of a given model, MC estimator, same as sample mean above, passing random vector 
# same for sample mean above 

totalLossComputation <- function(){
  loss <-0 
  for (i in 3:9 ){
    loss = loss + sum(data[i])
  }
  return (loss) 
}

totalPremium <- sum(data$PREMIUM) 
#tariff 


