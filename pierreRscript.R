source("rscript.r")
library(EnvStats)
library(ffbase)

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

#totalPremium <- sum(data$PREMIUM) 
#tariff 



#tentative ff plot
print(data[4,3])

lossesVector <- vector()
for  (i in 3:9 ){
  for (j in 1:length(data[,i])) 
    if ( data[j,i] > 0 ){
      lossesVector <- append(lossesVector, data[j,i])
      print (data[j,i])
    }
}
empiricalCDFVector <- vector() 



print(lossesVector) 


hist(lossesVector)

#epdfPlot(lossesVector, discrete = TRUE)


#exponential estimator method of moments
estimated_exponential_lambda <- 1/sample_mean(lossesVector)
print (estimated_exponential_lambda)


#prototype for FF plot of losses vector 
plot.ecdf(lossesVector, main = "FF plot", xlab = "Loss Amount", ylab = "Empirical Cumulative Distribution Function", col.points = rgb(0,0,0,0.25))
x = seq(0,1500, 0.1)
y = pexp(x, rate = estimated_exponential_lambda ) #gamma as an example 
lines(x,y, col = "blue")
#disqualifies exponential

#log normal disqu. by heavy tail, no hints of it here 

#gamma k and teta estimators by method of moments 
gamma_estimated_teta <- sample_variance(lossesVector) / sample_mean(lossesVector)
gamma_estimated_k <- sample_mean(lossesVector)/ gamma_estimated_teta 

#prototype for FF plot of losses vector , here in the gamma case 
plot.ecdf(lossesVector, main = "FF plot", xlab = "Loss Amount", ylab = "Empirical Cumulative Distribution Function", col.points = rgb(0.5,0,0,0.25))
x = seq(0,1500, 0.1)
y = pgamma(x, shape = gamma_estimated_k, scale =  gamma_estimated_teta) 
lines(x,y, col = "blue")
#pretty nice 
#exp special case of gamma so remove it 

#inverse gaussian distribution 
#estimators 

hist(lossesVector)
x = seq(0,1500, 0.1)
y = 400000*dgamma(x, shape = gamma_estimated_k, scale =  gamma_estimated_teta) #gamma as an example 
lines(x,y, col = "blue")

#weibul estimators
#Method of moments -> no exact solution 
#instead go for MLE for n observations 
#lamba <- ( sum of observation power k div by n ) power 1/ k 
#for k solution of lin equation 
# no direct way to solve such an equation in R 
# although eweibull(x, method = "mle") with x set of observations 
hist(lossesVector)

print(eweibull(x= lossesVector, method = "mle"))

names(eweibull(x= lossesVector, method = "mle")) 

estimate <- eweibull(x= lossesVector, method = "mle")

print(estimate$parameters)

names(estimate$parameters)

value <- estimate$parameters

print (value)

print (value[[1]]) #cannot access atomic vecors with $ 

print (value[[2]])

#prototype for FF plot of losses vector , here in the weibul case 
plot.ecdf(lossesVector, main = "FF plot", xlab = "Loss Amount", ylab = "Empirical Cumulative Distribution Function", col.points = rgb(0.5,0,0,0.25))
x = seq(0,1500, 0.1)
y = pweibull(x, shape = value[[1]], scale =  value[[2]]) 
lines(x,y, col = "blue")
y = pgamma(x, shape = gamma_estimated_k, scale =  gamma_estimated_teta) 
lines(x,y, col = "green")
#not as good as above 

shape <- eweibull(x= lossesVector, method = "mle")$parameters[[1]]
print (eweibull(x= lossesVector, method = "mle"))
print(shape)


hist(eweibull(x= lossesVector, method = "mle"))

x = seq(0,1500, 0.1)
y = 400000*dgamma(x, shape = gamma_estimated_k, scale =  gamma_estimated_teta) #gamma as an example 
lines(x,y, col = "blue")



# plot(x, ecdf(lossesVector)(x))

# plot(x, pgamma(x, shape= 9, scale= 0.5), add = TRUE)

# stepfun(x = ecdf(lossesVector), y = 1, right= true)


# plot(x = ChickWeight$weight[ChickWeight$Diet == 2],
#      breaks = 100,
#      col = gray(0, 0.5))


#empiricalClaimAmountCdf <- stepfun(x,y rcontin) 
# 
# hist(x = ChickWeight$weight[ChickWeight$Diet == 1],
#      main = "Two Histograms in one",
#      xlab = "Weight",
#      ylab = "Frequency",
#      breaks = 20,
#      xlim = c(0, 500),
#      col = gray(0, .5))
# 
# plot(x = ChickWeight$weight[ChickWeight$Diet == 2],
#      breaks = 30,
#      add = TRUE, # Add plot to previous one!
#      col = gray(1, .8))


ks_test_sum <-0 
for (i in 1:100){
  print (i) 
  simulatedGamma <- vector() 
  for (i in 1:length(lossesVector)){
    simulatedGamma <- append( simulatedGamma, rgamma(n= 1,shape = gamma_estimated_k, scale =  gamma_estimated_teta))
  }
  ks_test_sum <- ks_test_sum + ks.boot(lossesVector, simulatedGamma)$ks$p.value
}
print (ks_test_sum/100) 
#avg pvalue (print i above behaves strangely when placed at the end of the loop)
# avg for 10 as for 100 is approx 0.015 (0.01688333 for a 100 run) 


for (i in 1:length(lossesVector)){
  simulatedGamma <- append( simulatedGamma, rgamma(n= 1,shape = gamma_estimated_k, scale =  gamma_estimated_teta))
}
names(ks.boot(lossesVector, simulatedGamma))

object <- ks.boot(lossesVector, simulatedGamma)

print (object)

names(object)

print(object$ks$p.value)

#same for weibull and maybe for 

#simulation : poisson negbin exponential gamma weibull 
#above poisson and exp 

simulate_negative_binomial <- function(r, p, size){
  simulated_negative_binomial <- rep(0, size)
  for (i in 1:size){
    cat("item", i)
    lower_bound <- 0 
    upper_bound <- choose(counter + r - 1 , counter )* p^r * (1-p)^counter
    
    print(lower_bound) 
    print(upper_bound) 
    counter <- 0 
    random_number <- runif(1,0,1)
    print(random_number)
    while (! ((upper_bound > random_number ) & (lower_bound <= random_number) )) {
      print(" ")
      counter <- counter + 1 
      lower_bound <- upper_bound 
      upper_bound <- upper_bound +  choose(counter + r - 1 , counter )* p^r * (1-p)^counter
      cat("lower bound ", lower_bound, "   upper bound ", upper_bound, " random number ", random_number, "   counter ", counter)
    }
    simulated_negative_binomial[i] <- counter
  }
  return (simulated_negative_binomial)
}

print(simulate_negative_binomial(10, 0.5, 10))

#antithetic estimator attempt 

# poisson inversion method 

negative_binomial_inversion_method <- function(random_number, r, p) {
  counter <- 0 
  lower_bound <- 0 
  upper_bound <- choose(counter + r - 1 , counter )* p^r * (1-p)^counter
  while (! ((upper_bound > random_number ) & (lower_bound <= random_number) )) {
    counter <- counter + 1 
    lower_bound <- upper_bound 
    upper_bound <- upper_bound +  choose(counter + r - 1 , counter )* p^r * (1-p)^counter
    #cat("lower bound ", lower_bound, "   upper bound ", upper_bound, " random number ", random_number, "   counter ", counter)
    #print(" ")
  }
  return (counter)
}

negative_binomial_antithetic_estimator <- function (sample_size, r, p) {
  sum <- 0 
  for (i in 1:sample_size) {
    random_number <-  runif(1,0,1)
    sum <- sum + negative_binomial_inversion_method(random_number, r, p) + negative_binomial_inversion_method(1-random_number, r, p)  
  }
  sum <- sum /(2* sample_size )
}



poisson_inversion_method <- function(random_number, lambda) {
  counter <- 0 
  lower_bound <- 0 
  upper_bound <- dpois(counter,lambda=lambda) 
  while (! ((upper_bound > random_number ) & (lower_bound <= random_number) )) {
    counter <- counter + 1 
    lower_bound <- upper_bound 
    upper_bound <- upper_bound +  dpois(counter,lambda=lambda) 
    #cat("lower bound ", lower_bound, "   upper bound ", upper_bound, " random number ", random_number, "   counter ", counter)
    #print(" ")
  }
  return (counter)
}

poisson_antithetic_estimator <- function (sample_size, lambda) {
  sum <- 0 
  for (i in 1:sample_size) {
    random_number <-  runif(1,0,1)
    sum <- sum + poisson_inversion_method(random_number, lambda) + poisson_inversion_method(1-random_number, lambda)  
  }
  sum <- sum /(2* sample_size )
}



#no need for a funcion really 
gamma_inversion_method <- function (random_number, k, theta) {
  return (qgamma(random_number, shape = k, scale =  theta) )
}

gamma_antithetic_estimator <- function (sample_size, k, theta) {
  sum <- 0 
  for (i in 1:sample_size) {
    random_number <-  runif(1,0,1)
    sum <- sum + gamma_inversion_method(random_number, k, theta) + poisson_inversion_method(1-random_number, k, theta)  
  }
  sum <- sum /(2* sample_size )
}


#importance sampling -> optimal is disqualified ? g(x) is x is positive so e(abs val not known) (technically is but defeats the purpose )
# shifting and scalind fo not make for easier simulation, exponential twisting neither 

