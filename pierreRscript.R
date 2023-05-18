source("rscript.r")
library(EnvStats)
library(ffbase)
library(EnvStats)
library(statmod)
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
    #cat("item", i)
    counter <- 0 
    lower_bound <- 0 
    upper_bound <- choose(counter + r - 1 , counter )* p^r * (1-p)^counter
    #print(lower_bound) 
    #print(upper_bound) 
    random_number <- runif(1,0,1)
    #print(random_number)
    while (! ((upper_bound > random_number ) & (lower_bound <= random_number) )) {
      #print(" ")
      counter <- counter + 1 
      lower_bound <- upper_bound 
      upper_bound <- upper_bound +  choose(counter + r - 1 , counter )* p^r * (1-p)^counter
      #cat("lower bound ", lower_bound, "   upper bound ", upper_bound, " random number ", random_number, "   counter ", counter)
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

#vehicle discrimination 
# SUV     .  .    Sports Car.    Van 

vehicleType <- factor("Panel Truck", "Pickup", "Sedan", "Sports Car", "SUV", "Van") 

print(vehicleType)

class("Panel Truck")

levels(data[,11])

class(data[3,11][[1]]) 

class(data[3,11]) 

as.character(data[3,11])

data[3,11]


claimFrequencyOccurencesPerVehicleType <- matrix(0, 6, 8 )
for (i in 1:length(data[,1])){
  counter <- 1
  print (data[i,12])
  while (! (as.character(data[i,11]) == vehicleType[counter]) ){
    counter <- counter + 1 
  }
  claimFrequencyOccurencesPerVehicleType[counter , data[i,2] + 1 ] <- claimFrequencyOccurencesPerVehicleType[counter + 1 , data[i,2] + 1 ] + 1 
}
print(claimFrequencyOccurencesPerVehicleType)


plot.ecdf(lossesVector, main = "FF plot", xlab = "Loss Amount", ylab = "Empirical Cumulative Distribution Function", col.points = rgb(0,0,0,0.25))



hist(data[data$CAR_TYPE=="SUV","CLM_FREQ"], col = gray(0,0.5)) 
#hist(data[data$CAR_TYPE=="Sedan","CLM_FREQ"], col = gray(1,0.8), add = T) 

hist(data[data$CAR_TYPE=="Sports Car","CLM_FREQ"], col = gray(1,0.8)) 




     
plot.ecdf(data[data$CAR_TYPE=="SUV","CLM_FREQ"], col = gray(0,0.5))
plot.ecdf(data[data$CAR_TYPE=="Sedan","CLM_FREQ"], col = gray(1,0.8), add = T)



vehicleType <- c("Panel Truck", "Pickup", "Sedan", "Sports Car", "SUV", "Van") 
vehicleTypeAverage <- rep(0, 6 )
for (i in 1:length(vehicleType)){
  vehicleTypeAverage[i] <- mean(data[data$CAR_TYPE==vehicleType[i],"CLM_FREQ"])
}
print(vehicleTypeAverage)

vehicleType <- c("Panel Truck", "Pickup", "Sedan", "Sports Car", "SUV", "Van") 
vehicleTypeAverageClaimSeverity <- rep(0, 6 )
for (i in 1:length(vehicleType)){
  currentVector <- vector() 
  currentVector <- append(currentVector, data[data$CAR_TYPE==vehicleType[i],"CLM_AMT_1"]) 
  currentVector <- append(currentVector, data[data$CAR_TYPE==vehicleType[i],"CLM_AMT_2"]) 
  currentVector <- append(currentVector, data[data$CAR_TYPE==vehicleType[i],"CLM_AMT_3"]) 
  currentVector <- append(currentVector, data[data$CAR_TYPE==vehicleType[i],"CLM_AMT_4"]) 
  currentVector <- append(currentVector, data[data$CAR_TYPE==vehicleType[i],"CLM_AMT_5"]) 
  currentVector <- append(currentVector, data[data$CAR_TYPE==vehicleType[i],"CLM_AMT_6"]) 
  currentVector <- append(currentVector, data[data$CAR_TYPE==vehicleType[i],"CLM_AMT_7"]) 
  cleanUp <- vector() 
  for( j in 1:length(currentVector)){
    if (currentVector[j] > 0 ){
      cleanUp <- append(cleanUp, currentVector[j])
    }
  }
  hist(cleanUp)
  vehicleTypeAverageClaimSeverity[i] <- mean(cleanUp)
}
print(vehicleTypeAverageClaimSeverity)
# disappointing, disparity is not that great 

useType <- c("Private", "Commercial") 
useTypeAverageClaimSeverity <- rep(0, 2 )
useTypeVarianceClaimSeverity <- rep(0, 2 )

for (i in 1:length(useType)){
  currentVector <- vector() 
  currentVector <- append(currentVector, data[data$CAR_USE==useType[i],"CLM_AMT_1"]) 
  currentVector <- append(currentVector, data[data$CAR_USE==useType[i],"CLM_AMT_2"]) 
  currentVector <- append(currentVector, data[data$CAR_USE==useType[i],"CLM_AMT_3"]) 
  currentVector <- append(currentVector, data[data$CAR_USE==useType[i],"CLM_AMT_4"]) 
  currentVector <- append(currentVector, data[data$CAR_USE==useType[i],"CLM_AMT_5"]) 
  currentVector <- append(currentVector, data[data$CAR_USE==useType[i],"CLM_AMT_6"]) 
  currentVector <- append(currentVector, data[data$CAR_USE==useType[i],"CLM_AMT_7"]) 
  cleanUp <- vector() # remove the zeroes 
  for( j in 1:length(currentVector)){
    if (currentVector[j] > 0 ){
      cleanUp <- append(cleanUp, currentVector[j])
    }
  }
  hist(cleanUp) 
  useTypeAverageClaimSeverity[i] <- mean(cleanUp)
  useTypeVarianceClaimSeverity[i] <- var(cleanUp)
}
print(useTypeAverageClaimSeverity)
print(useTypeVarianceClaimSeverity) # variance more significant maybe 
# disappointing, disparity is not that great either 
# put together and do a versus plot 


areaType <- c("Urban", "Rural") 
areaTypeAverageClaimSeverity <- rep(0, 2 )
areaTypeVarianceClaimSeverity <- rep(0, 2 )
for (i in 1:length(areaType)){
  currentVector <- vector() 
  currentVector <- append(currentVector, data[data$AREA==areaType[i],"CLM_AMT_1"]) 
  currentVector <- append(currentVector, data[data$AREA==areaType[i],"CLM_AMT_2"]) 
  currentVector <- append(currentVector, data[data$AREA==areaType[i],"CLM_AMT_3"]) 
  currentVector <- append(currentVector, data[data$AREA==areaType[i],"CLM_AMT_4"]) 
  currentVector <- append(currentVector, data[data$AREA==areaType[i],"CLM_AMT_5"]) 
  currentVector <- append(currentVector, data[data$AREA==areaType[i],"CLM_AMT_6"]) 
  currentVector <- append(currentVector, data[data$AREA==areaType[i],"CLM_AMT_7"]) 
  cleanUp <- vector() # remove the zeroes 
  for( j in 1:length(currentVector)){
    if (currentVector[j] > 0 ){
      cleanUp <- append(cleanUp, currentVector[j])
    }
  }
  hist(cleanUp) 
  areaTypeAverageClaimSeverity[i] <- mean(cleanUp)
  areaTypeVarianceClaimSeverity[i] <- var(cleanUp)
}
print(areaTypeAverageClaimSeverity)
print(areaTypeVarianceClaimSeverity) # variance more significant maybe 
# disappointing, disparity is not that great either 
# put together and do a versus plot 



print(data[data$CAR_TYPE=="Pickup","CLM_AMT_1"] )


#use discrimination Private Commercial 

# age discrimination 19 to 80 

#urban vs Rural 

#covariance with categorical variables 

# get hypothesis on relevant categories with further data exploration 
# gender is illegal 

# intuitively : age classes for frequency, vehicle type for severity 

#claim frequency dist as function of age 

19 to 80 
16 categories 
- 3 
13 categories 
15 
20


#INITIALISE NUMBER OF AGE BRACKETS 
numberOfAgeBrackets <- 14
ageBracketVector <- rep(0, numberOfAgeBrackets + 1 )
for (i in 1:length(ageBracketVector)){
  ageBracketVector[i] <- 15 + (i-1)*5 
}  
#print (ageBracketVector) 
#COLLECT CLAIM FREQUENCY DATA AS FUNCTION OF AGE 
claimFrequencyOccurencesPerAgeBracket <- matrix(0, numberOfAgeBrackets, 8 )
print (claimFrequencyOccurencesPerAgeBracket) 
for (i in 1:length(data[,1])){
  counter <-1
  print (data[i,12]) 
  while (! ((data[i,12] > ageBracketVector[counter]) & (data[i,12] <= ageBracketVector[counter + 1 ] )) ){
    counter <- counter + 1 
  }
  claimFrequencyOccurencesPerAgeBracket[counter , data[i,2] + 1 ] <- claimFrequencyOccurencesPerAgeBracket[counter + 1 , data[i,2] + 1 ] + 1 
}
#technically no data in last bracket 
#print  (claimFrequencyOccurencesPerAgeBracket) 
claimFrequencyAveragePerAgeBracket <- rep(0, numberOfAgeBrackets)
for (i in 1:numberOfAgeBrackets){
  currentSum <-0 
  itemCounter <- 0 
  for (j in 1:8){
    currentSum <- currentSum + (j-1)*claimFrequencyOccurencesPerAgeBracket[i,j ]
    itemCounter <- itemCounter + claimFrequencyOccurencesPerAgeBracket[i,j ]
  }
  if (itemCounter > 0 ) {
    claimFrequencyAveragePerAgeBracket[i] <- currentSum/ itemCounter
  }
}
print (claimFrequencyAveragePerAgeBracket) 
#fake age bracket <- middle of age brackets 
fakeAgeBracketsVector <- rep(0, numberOfAgeBrackets)
for (i in 1:numberOfAgeBrackets ){
  fakeAgeBracketsVector[i] <- 17.5 + (i-1)*5 
}
plot(fakeAgeBracketsVector, claimFrequencyAveragePerAgeBracket, type = "b") 

# number of claim model -> should have a parameter informed by the age bracket ? function of age bracket 
# same for the claim severity model ... parameters as functions of car type 







# INITIALISE AGE BRACKETS 
ageBracketsVector <- vector() 
for (i in 1:13 ){
  print (i) 
  ageBracketsVector <- append(ageBracketsVector, 15 + 5*i )
}
#print( ageBracketsVector)
# INITIALISE AGE BRACKETS 
sumClaimFrequencyVectorPerAgeBracket <- vector() 
itemClaimFrequencyVectorPerAgeBracket <- vector(0, 0) 
for (j in 1:length(data[,2])) 
    if ( data[j,i] > 0 ){
      counter <-0 
      while (! ((data[j,12] > (15 + 5*counter)) & (data[j,12] <= (15 + 5*(counter+ 1 )) )) ){
        counter <- counter + 1 
      }
      claimFrequencyVectorPerAgeBracket[counter + 1 ] <- claimFrequencyVectorPerAgeBracket[counter + 1 ] + data[j,2]
      itemClaimFrequencyVectorPerAgeBracket[counter + 1 ] <- itemClaimFrequencyVectorPerAgeBracket[counter + 1 ] + 1 
    }
averageClaimFrequencyVectorPerAgeBracket <- vector() 
for (i in 1:length(sumClaimFrequencyVectorPerAgeBracket)  ){
  if (itemClaimFrequencyVectorPerAgeBracket[i] > 0 ) {
    averageClaimFrequencyVectorPerAgeBracket[i] <- sumClaimFrequencyVectorPerAgeBracket[i] / itemClaimFrequencyVectorPerAgeBracket[i]
  }
}

print(fakeAgeBracketsVector) 
print(length(fakeAgeBracketsVector) ) 
print(length(averageClaimFrequencyVectorPerAgeBracket))
plot (fakeAgeBracketsVector, averageClaimFrequencyVectorPerAgeBracket) 

print ( cov(data[,2], data[,12])) 



for  (i in 3:9 ){
  for (j in 1:length(data[,i])) 
    if ( data[j,i] > 0 ){
      counter <-0 
      while (! ((data[j,12] > (15 + 5*counter)) & (data[j,12] <= (15 + 5*(counter+ 1 )) )) ){
        counter <- counter + 1 
      }
      claimFrequencyVectorPerAgeBracket[counter + 1 ] <- claimFrequencyVectorPerAgeBracket[counter + 1 ] + 
    }
}



claimFrequencyVector <- vector()
for  (i in 3:9 ){
  for (j in 1:length(data[,i])) 
    if ( data[j,i] > 0 ){
      lossesVector <- append(lossesVector, data[j,i])
      print (data[j,i])
    }
}
empiricalCDFVector <- vector() 


#SOLVING THE WEIBULL FITTING ISSUE
placeHolder <- eweibull(x = lossesVector, method = "mle")

print(placeHolder)
# FIX = somehow when EnvStats is included last, it works ... 
#other option, ForestFit fitWeibull 
# to avoid numerical resolution 

#LOG NORMAL HOMEMADE FITTING 
#log normal method of moments parameters estimator
logNormalSigmaSquareEstimator <- function(random_vector){
  return(log(1 + sample_variance(random_vector)/(sample_mean(random_vector)^2)))
}
logNormalMuEstimator <- function(random_vector){
  return(log(sample_mean(random_vector)) - (logNormalSigmaSquareEstimator(random_vector)/2) )
}
sigmaSquareLN <- logNormalSigmaSquareEstimator(lossesVector)
muLN <- logNormalMuEstimator(lossesVector)

#simulation LN (does not allow for vectors yet)
simulatedStandardNormalDistribution <- function(){
  randomNumber1 <- runif(1,0,1)
  exponentialRandomNumber <- simulate_exponential(1, 1)
  while (randomNumber1 > exp(-0.5*(1+y)^2)) {
    randomNumber1 <- runif(1,0,1)
    exponentialRandomNumber <- simulate_exponential(1, 1)
  }
  randomNumber2 <- runif(1,0,1)
  if (randomNumber2 < 0.5 ) {
    return (-exponentialRandomNumber)
  }
  else {
    return (exponentialRandomNumber)
  }
}
simulatedLogNormalDistribution <- function(mu, sigmaSquared) {
  return (exp(sigmaSquared*simulatedStandardNormalDistribution() + mu ) ) 
}



#prototype for FF plot of losses vector, Log Normal case 
plot.ecdf(lossesVector, main = "FF plot", xlab = "Loss Amount", ylab = "Empirical Cumulative Distribution Function", col.points = "gray")
x = seq(0,1500, 0.1)
y = plnorm(x, meanlog = muLN, sdlog = sqrt(sigmaSquareLN) ) #sd log is sigma not sigma squared 
lines(x,y, col = "green")
z = pgamma(x, shape = gamma_estimated_k, scale =  gamma_estimated_teta) 
lines(x,z, col = "red")
u = pweibull(x, shape = value[[1]], scale =  value[[2]]) 
lines(x,u, col = "blue")
#LOOKS LIKE A MUCH BETTER FIT THAN GAMMA :( NEED SOME BACKTRACKING... GOOD NEWS -> TWO DECENT CANDIDATES FOR IMPORTANCE SAMPLING 


#PREMIUM FROM DATA 
# total losses divided by number of individual contracts 
riskPremiumFromData <- sum(lossesVector)/length(data$id)
print(riskPremiumFromData)
# total premium divided by number of individual contracts 
riskPremiumFromData2 <- sum(data$PREMIUM)/length(data$id)
print(riskPremiumFromData2)
# lower than sum of losses 

#TENTATIVE MC PREMIUM COMPUTATION 
riskPremiumFromMC <- function(numberOfContracts, numberOfIterations){
  simulationClaimsVector <- vector() 
  for (i in 1:numberOfIterations){
    sumOfClaimsPerIndividualContractVector <- vector() 
    numberOfClaimsVector <- simulateNumberOfClaims(parameters) #placeholder!!!
    for (j in 1:numberOfContracts){
      currentSumOfClaims <- 0 
      for (k in 1:numberOfClaimsVector[j]){
        currentSumOfClaims <- currentSumOfClaims + simulateClaimSeverity(parameters) #placeholder !!!
      }
      sumOfClaimsPerIndividualContractVector <- append(sumOfClaimsPerIndividualContractVector, currentSumOfClaims)
    }
    simulationClaimsVector <- append (simulationClaimsVector, sum(sumOfClaimsPerIndividualContractVector)) 
  } 
  return(sample_mean(simulationClaimsVector)) 
}




#inverse gaussian parameter estimation with the method of moments 
#mu lambda 
inverseGaussianMuEstimator <- function(randomVector) {
  return (sample_mean(randomVector)) 
} 
inverseGaussianLambdaEstimator <- function(randomVector) {
  return( sample_mean(randomVector)^3 / sample_variance(randomVector)) 
}

simulatedInverseGaussian <- function(mu, lambda){
  initialValue <- simulatedStandardNormalDistribution()^2
  secondaryValue <- mu + ((mu^2 * initialValue )/(2*lambda)) - (sqrt(4*lambda*mu*initialValue + mu^2 * initialValue^2 )*mu/(2*lambda))
  randomNumber <- runif(1,0,1)
  if (randomNumber <= (mu / (mu + x) ) ) {
    return(secondaryValue) 
  }
  else {
    return(mu^2 / secondaryValue)
  }
}  

muIG <- inverseGaussianMuEstimator(lossesVector)
lambdaIG <- inverseGaussianLambdaEstimator(lossesVector) 
print (muIG)
print (lambdaIG )

#prototype for FF plot of losses vector, Log Normal case 
plot.ecdf(lossesVector, main = "FF plot", xlab = "Loss Amount", ylab = "Empirical Cumulative Distribution Function", col.points = "gray")
x = seq(0,1500, 0.1)
y = plnorm(x, meanlog = muLN, sdlog = sqrt(sigmaSquareLN) ) #sd log is sigma not sigma squared 

z = pgamma(x, shape = gamma_estimated_k, scale =  gamma_estimated_teta) 
#lines(x,z, col = "red")
u = pweibull(x, shape = value[[1]], scale =  value[[2]]) 
#lines(x,u, col = "blue")
v = pinvgauss(x, mean = muIG , shape = lambdaIG) 
lines(x,v, col = "green")
lines(x,y, col = "red")
#INVERSE GAUSSIAN IS ALSO AN EXTREMELY GOOD FIT 
#NEED THE OTHER PLOTS + KS AND CHI SQUARED TO DECIDE/SELECT MODEL 
# in any case, very solid candidate for importance selection 




#TENTATIVE RUIN THEORY 
# alpha quantile to check  
crudeMCSimAlphaQuantile <- function(alpha, simulatedVector) {
  #initialise simulatedvector 
  # find percentage of vector 
  #integer casting of 
  orderedSimulatedVector <- sort(simulatedVector, decreasing= FALSE)
  index <- as.integer(alpha *length(orderedSimulatedVector)) #or  round(alpha *length(simulatedVector), digits = 0) 
  return(orderedSimulatedVector[index]) 
}
#test run 
quantileTestRunVector <- runif(1000, 0, 1 ) 

crudeMCSimAlphaQuantile(0.05, quantileTestRunVector)
# pretty good test quantile 

# had it been a stochastic process nice visual idea -> store all trajectories and draw all of them in gradient of color 

crudeMCExpectedShortfall <- function(alpha, numberOfIterations) { #number of policies as parameter ? 
  simulatedVector <- vector() 
  for (i in 1:numberOfIterations) {
    currentSimulation <- SIMULATEPLACEHOLDER()
    #either loop on number of policies OR call a function passing number of policies as an argument 
    simulatedVector <- append (simulatedVector, currentSimulation )
  }
  orderedSimulations <- sort(simulatedVector, decreasing= TRUE)  # check coherence with the above 
  selectionIndex <- ( floor(numberOfIterations * (1 - alpha ) ) + 1 ) 
  expectedShortfall <- 0 
  for (i in 1:selectionIndex ){
    expectedShortfall <- expectedShortfall +  (orderedSimulations[i]/ ( floor(numberOfIterations * (1 - alpha ) ) + 1 ) )
  }
}
#fits course description, need to actually run tests 


#Finish expected shortfall 







